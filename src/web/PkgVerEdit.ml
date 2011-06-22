
(** Edit package's version 
   
    @author Sylvain Le Gall
  *)

open Lwt
open Context

let start_edit ~ctxt sp ((pkg_str, ver) as k) edited =
  let mem_fs = 
    new ODBFSMemory.read_write (ODBFSTree.root ())
  in
  let stor_fs = 
    ODBStorage.fs ctxt.stor
  in
    ODBFSMemory.cp_directories stor_fs mem_fs
    >>= fun () ->
    PkgVerEditCommon.start_edit ~ctxt sp 
      {PkgVerEditCommon.
       edt_hsh_key  = k;
       edt_hsh_sess = edited;
       edt_mem_fs   = mem_fs;
       edt_stor_fs  = stor_fs;
       edt_is_new   = false;
       edt_pkg_str  = pkg_str;
       edt_ver      = ver;
       edt_ver_ko   = ver;
       edt_commit   = (fun () -> return ());
      }

let rec handler sp k () = 
    Context.get_user ~sp ()
    >>= fun (ctxt, accnt) ->
    begin
      let edited = 
        Hashtbl.create 13
      in
        (* This new service will override future call to the service *)
        Eliom_predefmod.Xhtml.register_for_session 
          ~sp
          ~service:Common.edit_pkg_ver
          (fun sp k _ ->
             try
               let fpage = 
                 Hashtbl.find edited k 
               in
                 fpage ()
             with Not_found ->
               start_edit ~ctxt sp k edited);

        (* First time, display the page *)
        start_edit ~ctxt sp k edited
    end

