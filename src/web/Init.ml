
open Lwt
open ODBGettext
open Eliom_services
open Eliom_parameters
open Eliom_predefmod
open Common

let () =
  try 
    let ctxt = 
      Context.read_config ();
      Context.get_odb ()
    in
    let _bkgrnd_job = 
      ODBMessage.info ~ctxt
        (f_ "OASIS-DB v%s started")
        ODBConf.version
      >>= 
      (* TODO: wait for completion *)
      ODBMain.run ~ctxt 
    in
      Xhtml.register home Index.home_handler;
      Xhtml.register browse Browse.browse_handler;
      Xhtml.register view Browse.view_handler;
      Redirection.register upload Upload.upload_handler;
      Xhtml.register contribute Index.contribute_handler;
      Xhtml.register about Index.about_handler;
      () 
  with e ->
    Printf.eprintf 
      (f_ "E: Exception raised during initialization: %s\n%!")
      (Printexc.to_string e)

let default = 
  (* Default = home *)
  Redirection.register_new_service
    ~path:[""]
    ~get_params:unit
    (fun sp () () ->
       return home)
