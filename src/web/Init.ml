
open Lwt
open ODBGettext

let run () = 
  let ctxt = 
    Context.get ()
  in
  let _bkgrnd_job = 
    ODBMessage.info ~ctxt 
      (f_ "OASIS-DB v%s started")
      ODBConf.version
    >>= 
    ODBMain.run ~ctxt 
  in
    Template.init ();
    Account.init ();
    MyAccount.init ();
    Browse.init ();
    Dist.init ();
    Upload.init ();
    Index.init ()

let register =
  Eliom_services.register_eliom_module "oasis-db-ocsigen" run
