
(** Create a new package's version derived from another one

    @author Sylvain Le Gall
  *)

open Lwt
open OASISVersion
open ODBGettext
open ODBPkgVer
open XHTML.M
open Eliom_predefmod.Xhtml
open Template
open Context


let box ~sp ~ctxt pkg_ver = 
  ODBStorage.PkgVer.elements ctxt.stor (`PkgVer pkg_ver)
  >|= fun pkg_ver_lst ->
  begin
    let ver_lst =
      List.map (fun pkg_ver -> pkg_ver.ver) pkg_ver_lst
    in
    let new_ver = 
      ODBDerive.version ver_lst pkg_ver.ver
    in
      match Account.get_id ~ctxt () with 
        | Some id ->
            a Common.derive_pkg_ver sp 
              [pcdata 
                 (Printf.sprintf 
                    (f_ "Derive v%s")
                    (string_of_version new_ver))]
              (pkg_ver.pkg, (pkg_ver.ver, new_ver))
        | None ->
            (* TODO: redirect link for login *)
            span [pcdata 
                    (Printf.sprintf
                       (f_ "Derive v%s")
                       (string_of_version new_ver))]
  end

let start_edit ~ctxt sp ((pkg_str, (from_ver, to_ver)) as k) derived =
  let mem_fs = 
    new ODBFSMemory.read_write (ODBFSTree.root ())
  in
  let stor_fs = 
    ODBStorage.fs ctxt.stor
  in
    ODBFSMemory.cp_directories stor_fs mem_fs
    >>= fun () ->
    begin
      let phantom_fs = new ODBVFSUnion.read_write mem_fs [stor_fs] 
      in
        ODBStorage.create_read_write ~ctxt:ctxt.odb phantom_fs
        >>= fun stor -> 
        ODBStorage.PkgVer.derive stor (`StrVer (pkg_str, from_ver)) to_ver
        >>= fun () -> 
        PkgVerEditCommon.start_edit ~ctxt sp 
          {PkgVerEditCommon.
           edt_hsh_key  = k;
           edt_hsh_sess = derived;
           edt_mem_fs   = mem_fs;
           edt_stor_fs  = stor_fs;
           edt_is_new   = true;
           edt_pkg_str  = pkg_str;
           edt_ver      = to_ver;
           edt_ver_ko   = from_ver;
           edt_commit   = 
             (fun () -> 
                ODBStorage.PkgVer.update_tarball 
                  stor 
                  (`StrVer (pkg_str, to_ver)))
          }
    end

let handler sp k () = 
  Context.get_user ~sp () 
  >>= fun (ctxt, _) ->
  begin
    let derived = 
      Hashtbl.create 13
    in

      (* This new service will override future call to the service *)
      Eliom_predefmod.Xhtml.register_for_session 
        ~sp
        ~service:Common.derive_pkg_ver
        (fun sp k _ ->
           try
             let fpage = 
               Hashtbl.find derived k 
             in
               fpage ()
           with Not_found ->
             start_edit ~ctxt sp k derived);

      (* First time, display the page *)
      start_edit ~ctxt sp k derived 
  end
