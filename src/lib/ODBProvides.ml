
open OASISUtils
open ODBVer
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


let map ~ctxt () = 
  ODBStorage.Pkg.elements () 
  >>= 
  begin
    let one_ver ver = 
      ODBStorage.Ver.filename 
        ver.pkg 
        (OASISVersion.string_of_version ver.ver) 
        `OASIS
      >>= fun fn -> 
      catch 
        (fun () ->
           ODBOASIS.from_file ~ctxt fn
           >|= 
           of_oasis_package
           >|=
           List.rev_map (fun prvd -> prvd, ver))
        (fun _ -> 
           return [])
    in

    let one_pkg pkg = 
      ODBStorage.Ver.elements pkg
      >>=
      Lwt_list.rev_map_p one_ver 
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

