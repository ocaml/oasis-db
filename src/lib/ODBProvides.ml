
open OASISUtils
open ODBPkg
open ODBPkgVer
open Lwt

(* TODO: tool dependencies which includes test/doc, for dependencies
 * and provides.
 *)
let of_oasis_package pkg =
  MapString.fold
    (fun _ vl acc ->
       let hd = 
         match vl with 
           | Some pre, nm -> 
               pre^"."^nm
           | None, nm -> 
               nm
       in
         hd :: acc)
    (OASISLibrary.findlib_name_map pkg)
    []


let map ~ctxt stor = 
  ODBStorage.Pkg.elements stor
  >>= 
  begin
    let one_pkg_ver pkg_ver = 
      ODBStorage.PkgVer.oasis stor (`PkgVer pkg_ver) 
      >|= 
        function 
          | Some oasis ->
              List.rev_map 
                (fun prvd -> prvd, pkg_ver)
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

  (* We have a list of package version list, create a map *)
  >|= fun lst_lst ->
  begin
    List.fold_left
      (List.fold_left
         (List.fold_left
            (fun mp (fndlb_nm, ver) ->
               let prev_lst = 
                 try 
                   MapString.find fndlb_nm mp
                 with Not_found ->
                   []
               in
                 MapString.add fndlb_nm (ver :: prev_lst) mp)))
      MapString.empty
      lst_lst
  end

