
(** Solve the dependencies of a package 
   
    @author Sylvain Le Gall
  *)

open OASISUtils
open OASISVersion
open ODBPkg
open ODBRepository
open ODBGettext
open Lwt

type 'a elt =
    {
      slv_repo: ODBRepository.t;
      slv_stor: 'a ODBStorage.t;
      slv_pkg_vers: (ODBPkgVer.t * OASISTypes.package option) list;
    }

type 'a t = ('a elt * 'a elt list) MapString.t

let default = MapString.empty

let add_repository ~ctxt repo fs t = 
  (* Build a list of available package version. Take into account
   * repository priority.
   *)
  ODBStorage.create_read_only ~ctxt fs
  >>= fun stor ->
  ODBStorage.Pkg.elements stor 
  >>= fun pkg_lst ->
  Lwt_list.rev_map_s
    (fun pkg ->
       ODBStorage.PkgVer.elements stor (`Pkg pkg)
       >>= fun lst ->
       Lwt_list.map_s 
         (fun pkg_ver ->
            ODBStorage.PkgVer.oasis stor (`PkgVer pkg_ver)
            >|= fun oasis_opt ->
            pkg_ver, oasis_opt)
         lst
       >|= fun lst ->
       pkg.pkg_name, 
       {slv_repo     = repo;
        slv_stor     = stor;
        slv_pkg_vers = lst})
    pkg_lst
  >|= fun lst ->
  begin
    let lst_sorted = 
      List.sort 
        (fun (pkg1, _) (pkg2, _) ->
           String.compare pkg1 pkg2)
        lst
    in
    let rec add_ordered pkg_str (elt, others) (lst, t) =
      match lst with 
        | (pkg_str', elt') :: tl ->
            if pkg_str' = pkg_str then
              begin
                (* We find a match, compare repository priority to know
                 * what to do 
                 *)
                if elt.slv_repo.repo_priority < elt'.slv_repo.repo_priority then
                  tl, MapString.add pkg_str (elt', elt :: others) t 
                else
                  tl, MapString.add pkg_str (elt, elt' :: others) t
              end
            else if pkg_str' < pkg_str then
              begin
                (* We are beyond the pkg_str, consume an element and 
                 * continue 
                 *)
                let t = 
                  MapString.add pkg_str' (elt', []) t
                in
                  add_ordered pkg_str (elt, others) (tl, t)
              end
            else
              begin
                (* We pass the pkg_str value, no need to consume lst *)
                lst, MapString.add pkg_str (elt, others) t
              end

        | [] ->
            [], MapString.add pkg_str (elt, others) t
    in
    let (lst_remains, t) = 
      MapString.fold add_ordered t (lst_sorted, default)
    in
      List.fold_left
        (fun t (pkg_str, elt) ->
           MapString.add pkg_str (elt, []) t)
        t
        lst_remains
  end

module MapPkgVer = Map.Make(ODBPkgVer) 

module SetOASISPkgVer =
  Set.Make
    (struct
       type t = 
           [ `ExternalTool of string 
           | `FindlibPackage of string * OASISVersion.comparator option 
           | `OASISPkgVer of OASISTypes.package_name * OASISVersion.t]
       let compare e1 e2 =
         let order = 
           function 
             | `ExternalTool _ -> 0
             | `FindlibPackage _ -> 1
             | `OASISPkgVer _ -> 2
         in
         match e1, e2 with 
           | `OASISPkgVer (nm1, ver1), `OASISPkgVer (nm2, ver2) ->
               begin
                 match String.compare nm1 nm2 with 
                   | 0 -> OASISVersion.version_compare ver1 ver2 
                   | n -> n
               end
           | `ExternalTool nm1, `ExternalTool nm2 
           | `FindlibPackage (nm1, _), `FindlibPackage (nm2, _) ->
               String.compare nm1 nm2 
           | e1, e2 ->
               (order e1) - (order e2)
     end)

let solve t preset_data oasis =

  let provides, oasis_pkgs = 
    (* Build a map of provides *)
    MapString.fold
      (fun pkg_str (elt, _) (prvd_mp, oasis_mp) ->
         List.fold_left 
           (fun (prvd_mp, oasis_mp) (pkg_ver, oasis_opt) ->
              match oasis_opt with 
                | Some oasis -> 
                    begin
                      let prvds = 
                        ODBProvides.of_oasis_package preset_data oasis 
                      in
                      let prvd_mp =
                        List.fold_left 
                          (fun prvd_mp (prvd, status) ->
                             let prev_lst = 
                               try 
                                 ODBProvides.Map.find prvd prvd_mp
                               with Not_found ->
                                 []
                             in
                               ODBProvides.Map.add prvd 
                                 ((status, pkg_ver) :: prev_lst)
                                 prvd_mp)
                          prvd_mp prvds
                      in
                      let oasis_mp = MapPkgVer.add pkg_ver oasis oasis_mp in
                        prvd_mp, oasis_mp
                    end
                | None ->
                    prvd_mp, oasis_mp)
           (prvd_mp, oasis_mp)
           elt.slv_pkg_vers)
      t
      (ODBProvides.Map.empty, MapPkgVer.empty)
  in

  let graph = OASISGraph.create 13 in

  let id oasis = 
    `OASISPkgVer (oasis.OASISTypes.name, oasis.OASISTypes.version) 
  in

  let rec solve_oasis set vrtx oasis = 
    let deps = 
      ODBDeps.solve 
        (ODBDeps.of_oasis_package preset_data oasis)
        provides
    in
      ODBDeps.Map.fold
        (fun dep dep_elt set ->
           match dep_elt.ODBDeps.package_version with 
             | Some pkg_ver ->
                 begin
                   try 
                     let oasis = MapPkgVer.find pkg_ver oasis_pkgs in
                     let id = id oasis in
                       if SetOASISPkgVer.mem id set then
                         begin
                           (* Already solved, no need to go further *)
                           set
                         end
                       else
                         begin
                           let elt, _ = 
                             MapString.find pkg_ver.ODBPkgVer.pkg t 
                           in
                           let set = SetOASISPkgVer.add id set in
                           let vrtx' = 
                             OASISGraph.add_vertex graph 
                               (`Node (pkg_ver, elt.slv_stor, oasis))
                           in
                             OASISGraph.add_edge graph vrtx vrtx';
                             solve_oasis set vrtx' oasis
                         end
                   with Not_found ->
                     failwithf
                       (f_ "Package %s v%s is a dependency \
                            of %s v%s but has no _oasis file or cannot be found \
                            in repositories.")
                       pkg_ver.ODBPkgVer.pkg
                       (OASISVersion.string_of_version pkg_ver.ODBPkgVer.ver)
                       oasis.OASISTypes.name
                       (OASISVersion.string_of_version oasis.OASISTypes.version)
                 end
             | None ->
                 begin
                   let ext =
                     match dep with 
                       | `FindlibPackage nm ->
                           Printf.eprintf "WWW: findlib package %s\n%!" nm;
                           `FindlibPackage (nm, dep_elt.ODBDeps.version_cmp)
                       | `ExternalTool nm ->
                           `ExternalTool nm
                   in
                     SetOASISPkgVer.add ext set
                 end)
        deps set
  in

  let set_deps = 
    solve_oasis 
      (SetOASISPkgVer.singleton (id oasis))
      (OASISGraph.add_vertex graph `Root)
      oasis
  in

  let _t : (OASISTypes.package_name * OASISVersion.t) option = 
    (* Check that there is not twice a package with different version. *)
    SetOASISPkgVer.fold
      (fun dep opt_prev ->
         match dep, opt_prev with 
           | `OASISPkgVer(nm, ver), Some (nm', ver') -> 
               begin
                 if nm <> nm' then
                   Some (nm, ver) 
                 else 
                   failwithf 
                     (f_ "Need to use at the same time v%s and v%s of package %s")
                     (OASISVersion.string_of_version ver)
                     (OASISVersion.string_of_version ver')
                     nm
               end
           | `OASISPkgVer (nm, ver), None ->
               begin
                 Some (nm, ver)
               end
           | _ ->
               opt_prev)
      set_deps None
  in

  let unsolved_deps =
    SetOASISPkgVer.fold
      (fun dep acc ->
         match dep with 
           | `ExternalTool nm ->
               (`ExternalTool nm) :: acc
           | `FindlibPackage (nm, ver_cmp) ->
               Printf.eprintf "III: findlib package %s\n%!" nm;
               (`FindlibPackage (nm, ver_cmp)) :: acc
           | `OASISPkgVer _ ->
               acc)
      set_deps
      []
  in

  let ordered_deps = 
    List.rev
      (List.fold_left 
         (fun acc vrtx ->
            match OASISGraph.value_of_vertex graph vrtx with
              | `Node (pkg_ver, slv_stor, oasis) -> 
                  (pkg_ver, slv_stor, oasis) :: acc
              | `Root -> acc)
         []
         (OASISGraph.topological_sort graph))
  in
    ordered_deps, unsolved_deps
