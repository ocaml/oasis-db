
open Lwt
open Lwt_log

type context =
    {
      odb:   ODBContext.t;
      role:  Account.t;

      upload_delay: float; 
      (* Delay for upload (wait for completion and refresh) *)   

      upload_commit_delay: float;
      (* Delay for an upload's commit (final step of upload) *)

      upload_cancel_delay: float;
      (* Delay to cancel an upload *)
    }

let get_odb () = 
  let storage_fn = 
    (* TODO: configure *)
    FilePath.make_filename ["test"; "data"; "storage"]
  in

  let logger = 
    Lwt_log.make
      ~output:
      (fun sct lvl lst ->
         (* TODO: add section info *)

         (* Log to output *)
         let task_stdout = 
           Lwt_list.iter_s 
             Lwt_io.printl             
             lst
         in
         let task_logfile = 
           let f = 
             match lvl with 
               | Error | Fatal -> 
                   Ocsigen_messages.errlog
               | Warning ->
                   Ocsigen_messages.warning
               | Notice | Info | Debug ->
                   ignore
           in
             Lwt_list.iter_s 
               (fun ln -> 
                  return (f ln))
               lst 
         in
         let task_db = 
           Log.add lvl 
             Log.Other 
             (Lwt_log.Section.name sct) 
             ("nothing", []) 
             lst
         in
           join [task_stdout; task_logfile; task_db])
      ~close:(fun () -> return ())
  in

    ODBContext.default ~logger storage_fn

let get ~sp () = 
  Account.get ~sp () 
  >>= fun role ->
  return 
    {
      odb  = get_odb ();
      role = role;

      (* TODO: load configuration *)
      upload_delay = 5.0;
      upload_commit_delay = 5.0;
      upload_cancel_delay = 5.0;
    }

let get_user ~sp () = 
  get ~sp () 
  >>= fun ctxt -> 
    match ctxt.role with 
      | Account.User accnt | Account.Admin accnt ->
          return (ctxt, accnt)
      | Account.Anon ->
          fail Common.RequiresAuth
