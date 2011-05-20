
open OASISUtils
open OASISTypes
open Lwt

type version = OASISVersion.t

type key =
  [ `ExternalTool of prog * version option 
  | `FindlibPackage of findlib_full * version]

module Map = 
struct
  module FullMap = 
    Map.Make
      (struct
         type t = key
         let compare = Pervasives.compare
       end)

  module NameMap = 
    Map.Make
      (struct 
         type t = [ `ExternalTool of prog
                  | `FindlibPackage of findlib_full]
         let compare = Pervasives.compare
       end)

  type 'a t = 
      {
        name_map: (key list) NameMap.t;
        full_map: 'a FullMap.t;
      }

  let empty =
    {
      name_map = NameMap.empty;
      full_map = FullMap.empty;
    }

  let name_of_key = 
    function
      | `ExternalTool (nm, _) -> `ExternalTool nm
      | `FindlibPackage (nm, _) -> `FindlibPackage nm 

  let add k v t =
    if FullMap.mem k t.full_map then
      (* Key already in, no need to add it to NameMap *)
      {t with full_map = FullMap.add k v t.full_map}
    else
      begin
        let nm = name_of_key k in
        let frmr = 
          try 
            NameMap.find nm t.name_map 
          with Not_found ->
            []
        in
          {
            name_map = NameMap.add nm (k :: frmr) t.name_map;
            full_map = FullMap.add k v t.full_map;
          }
      end

  let find k t = 
    FullMap.find k t.full_map

  let find_name ?ver_cmp nm t =
    let lst = 
      NameMap.find nm t.name_map
    in
    let lst = 
      match ver_cmp with 
        | Some cmp ->
            List.filter
              (function 
                 | `ExternalTool (_, Some ver) 
                 | `FindlibPackage (_, ver) ->
                     OASISVersion.comparator_apply ver cmp
                 | `ExternalTool (_, None) ->
                     true)
              lst
        | None ->
            lst
    in
      List.map (fun k -> FullMap.find k t.full_map) lst
end


let of_oasis_package pkg =
  (* TODO: precompute flags *)
  let fndlb_mp = 
    OASISLibrary.findlib_name_map pkg
  in
    List.fold_left
      (fun acc -> 
         function 
           | Executable (cs, bs, exec) ->
               begin
                 match OASISBuildSectionExt.installable pkg bs with 
                   | `Always | `Sometimes as status ->
                       (`ExternalTool (cs.cs_name, Some pkg.version), status) 
                       :: 
                       acc
                   | `Never ->
                       acc
               end
           | Library (cs, bs, lib) ->
               begin
                 match OASISBuildSectionExt.installable pkg bs with 
                   | `Always | `Sometimes as status ->
                       begin
                         try 
                           let fndlb_nm = 
                             match MapString.find cs.cs_name fndlb_mp with 
                               | Some pre, suf ->
                                   pre^"."^suf
                               | None, nm ->
                                   nm
                           in
                             (`FindlibPackage (fndlb_nm, pkg.version), status) 
                             :: 
                             acc
                         with Not_found ->
                           acc
                       end

                   | `Never ->
                       acc
               end
           | Flag _ | Test _ | SrcRepo _ | Doc _ ->
               acc)
      []
      pkg.sections



let map stor = 
  ODBStorage.Pkg.elements stor
  >>= 
  begin
    let one_pkg_ver pkg_ver = 
      ODBStorage.PkgVer.oasis stor (`PkgVer pkg_ver) 
      >|= 
        function 
          | Some oasis ->
              List.rev_map 
                (fun (prvd, status) -> (prvd, (status, pkg_ver)))
                (of_oasis_package oasis)
          | None ->
              []
    in

    let one_pkg pkg = 
      ODBStorage.PkgVer.elements stor (`Pkg pkg)
      >>=
      Lwt_list.rev_map_p one_pkg_ver 
    in

      Lwt_list.rev_map_p one_pkg 
  end

  (* We have a list of provides list, create a map *)
  >|= fun lst_lst ->
  begin
    List.fold_left
      (List.fold_left
         (List.fold_left
            (fun mp (prvd, data) ->
               let prev_lst = 
                 try 
                   Map.find prvd mp
                 with Not_found ->
                   []
               in
                 Map.add prvd (data :: prev_lst) mp)))
      Map.empty
      lst_lst
  end

