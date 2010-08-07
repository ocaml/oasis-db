
open Lwt
open ODBGettext

let run () = 
  let ctxt = 
    Context.get_odb ()
  in
  let _bkgrnd_job = 
    ODBMessage.info ~ctxt
      (f_ "OASIS-DB v%s started")
      ODBConf.version
    >>= 
    ODBMain.run ~ctxt 
  in
    AccountStub.init ();
    Account.init ();
    Template.init ();
    MyAccount.init ();
    NewAccount.init ();
    Browse.init ();
    Dist.init ();
    Upload.init ();
    Index.init ()

let register =
  Eliom_services.register_eliom_module "oasis-db-ocsigen" run
