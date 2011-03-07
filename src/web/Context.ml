
open Lwt
open Lwt_log
open Simplexmlparser
open ODBGettext

type context =
    {
      odb:   ODBContext.t;
      role:  Account.t;
      sqle:  Sqlexpr.t;

      upload_delay: float; 
      (* Delay for upload (wait for completion and refresh) *)   

      upload_commit_delay: float;
      (* Delay for an upload's commit (final step of upload) *)

      upload_cancel_delay: float;
      (* Delay to cancel an upload *)
    }

let mk_var nm = 
  ref 
    (fun () -> 
       failwith 
         (Printf.sprintf 
            "Uninitialized variable '%s'"
            nm))


let incoming_dir = mk_var "incoming directory"
let dist_dir     = mk_var "dist directory"
let sqle_fn      = mk_var "SQLite DB"
let sqle         = mk_var "SQLExpr context"

let init_mutex   = Lwt_mutex.create () 
let init_cond    = Lwt_condition.create () 
let init_val     = ref false


let read_config () = 
  let spf fmt = 
    Printf.sprintf fmt 
  in

  let rec parse lst =
    match lst with 
      | e :: tl ->
          begin
            let () = 
              match e with 
                | Element ("dir", ["rel", "incoming"], [PCData dn]) ->
                    incoming_dir := fun () -> dn
                | Element ("dir", ["rel", "dist"], [PCData dn]) ->
                    dist_dir := fun () -> dn
                | Element ("db", [], [PCData fn]) ->
                    sqle_fn := fun () -> fn
                | Element (nm, _, _) ->
                    failwith 
                      (spf
                         (f_ "Don't know what to do with \
                              configuration element '<%s .../>'") 
                         nm)
                | PCData str ->
                    failwith 
                      (spf
                         (f_ "Don't know what to do with \
                              configuration pcdata '%s'")
                         str)
            in
              parse tl
          end
      | [] ->
          ()
  in
  let test_dir fmt rfdn = 
    let dn =
      !rfdn ()
    in
      if not (Sys.is_directory dn) then
        failwith 
          (spf fmt dn)
  in

    parse (Eliom_sessions.get_config ());

    test_dir 
       (f_ "Incoming directory '%s' doesn't exist")
       incoming_dir;
    test_dir
      (f_ "Dist directory '%s' doesn't exist")
      dist_dir;
    ignore (!sqle_fn ())

let get_odb () = 
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
           let lvl = 
             match lvl with
               | Fatal   -> `Fatal
               | Error   -> `Error
               | Warning -> `Warning
               | Notice  -> `Notice
               | Info    -> `Info
               | Debug   -> `Debug
           in
             try 
               let sqle = 
                 !sqle ()
               in
                 Lwt_list.iter_s
                   (fun msg ->
                      Log.add sqle
                        (`Sys (Section.name sct, `Message(lvl, msg))))
                   lst
             with _ ->
               return ()
         in
           join [task_stdout; task_logfile; task_db])
      ~close:(fun () -> return ())
  in
    ODBContext.default ~logger 
      (!dist_dir ()) 
      (!incoming_dir ())

let init () = 
  Lwt_mutex.with_lock init_mutex
    (fun () ->
       begin
         let log =
           ODBMessage.info ~ctxt:(get_odb ()) "%s"
         in
           Sqlexpr.create ~log (!sqle_fn ())
       end
       >>= fun sqle' ->
       begin
         sqle := (fun () -> sqle');
         init_val := true;
         return ()
       end)
  >|= 
  Lwt_condition.broadcast init_cond

let get ~sp () = 
  Lwt_mutex.with_lock init_mutex
    (fun () ->
       begin
         if !init_val then
           return ()
         else
           Lwt_condition.wait ~mutex:init_mutex init_cond
       end
       >>= fun () ->
       Account.get ~sp () 
       >>= fun role ->
       return 
         {
           odb  = get_odb ();
           role = role;
           sqle = !sqle ();

           (* TODO: load configuration *)
           upload_delay = 5.0;
           upload_commit_delay = 5.0;
           upload_cancel_delay = 5.0;
         })

let get_user ~sp () = 
  get ~sp () 
  >>= fun ctxt -> 
    match ctxt.role with 
      | Account.User accnt | Account.Admin accnt ->
          return (ctxt, accnt)
      | Account.Anon ->
          fail Common.RequiresAuth

let get_admin ~sp () = 
  get ~sp () 
  >>= fun ctxt -> 
    match ctxt.role with 
      | Account.Admin accnt ->
          return (ctxt, accnt)
      | Account.Anon | Account.User _ ->
          fail Common.RequiresAuth
