
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
  ODBStorage.create ~ctxt fs (fun ~timestamp _ -> return ()) []
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

let solve t oasis =
  let provides = 
    (* Build a map of provides *)
    MapString.fold
      (fun pkg_str (elt, _) mp ->
         List.fold_left 
           (fun mp (pkg_ver, oasis_opt) ->
              match oasis_opt with 
                | Some oasis -> 
                    begin
                      let prvds = 
                        ODBProvides.of_oasis_package oasis 
                      in
                        List.fold_left 
                          (fun mp (prvd, status) ->
                             let prev_lst = 
                               try 
                                 ODBProvides.Map.find prvd mp
                               with Not_found ->
                                 []
                             in
                               ODBProvides.Map.add prvd 
                                 ((status, pkg_ver) :: prev_lst)
                                 mp)
                          mp
                          prvds
                    end
                | None ->
                    mp)
           mp
           elt.slv_pkg_vers)
      t
      ODBProvides.Map.empty
  in

  let deps = 
    ODBDeps.solve 
      (ODBDeps.of_oasis_package oasis)
      provides
  in

    (* TODO: ordered build *)
    (* Reattribute package to their original store *)
    ODBDeps.Map.fold
      (fun dep dep_elt (pkg_ver_lst, unsolved_deps) -> 
         match dep_elt.ODBDeps.package_version with 
           | Some pkg_ver -> 
               begin
                 try 
                   let elt, _ = 
                     MapString.find pkg_ver.ODBPkgVer.pkg t
                   in
                     (pkg_ver, elt.slv_stor) :: pkg_ver_lst, unsolved_deps
                 with Not_found ->
                   failwith 
                     (Printf.sprintf
                        (f_ "Unable to find package's version %s v%s")
                        pkg_ver.ODBPkgVer.pkg
                        (OASISVersion.string_of_version pkg_ver.ODBPkgVer.ver))
               end
           | None ->
               begin
                 let ext =
                   match dep with 
                     | `FindlibPackage nm ->
                         `FindlibPackage (nm, dep_elt.ODBDeps.version_cmp)
                     | `ExternalTool nm ->
                         `ExternalTool nm
                 in
                   pkg_ver_lst, ext :: unsolved_deps
               end)
      deps
      ([], [])
  
