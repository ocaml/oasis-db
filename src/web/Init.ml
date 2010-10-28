
open Lwt
open ODBGettext
open Types
open Eliom_services
open Eliom_parameters
open Eliom_predefmod
open Common

let () =
  let ctxt = 
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
    Xhtml.register upload Upload.upload_handler;
    Xhtml.register contribute Index.contribute_handler;
    Xhtml.register about Index.about_handler;
    () 

let default = 
  (* Default = home *)
  Redirection.register_new_service
    ~path:[""]
    ~get_params:unit
    (fun sp () () ->
       return home)
