
open Lwt
open Lwt_log
open Simplexmlparser
open ODBGettext

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

let incoming_dir = ref None
let dist_dir     = ref None

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
                    incoming_dir := Some dn
                | Element ("dir", ["rel", "dist"], [PCData dn]) ->
                    dist_dir := Some dn
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
  let test_dir not_defined fmt = 
    function 
      | Some dn -> 
          if not (Sys.is_directory dn) then
            failwith 
              (spf fmt dn)
      | None ->
          failwith not_defined
  in

    parse (Eliom_sessions.get_config ());

    test_dir 
      "Incoming directory not defined"
       (f_ "Incoming directory '%s' doesn't exist")
       !incoming_dir;
    test_dir
      "Dist directory not defined"
      (f_ "Dist directory '%s' doesn't exist")
      !dist_dir


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
           Log.add lvl 
             Log.Other 
             (Lwt_log.Section.name sct) 
             ("nothing", []) 
             lst
         in
           join [task_stdout; task_logfile; task_db])
      ~close:(fun () -> return ())
  in

  let the = 
    function 
      | Some e -> e
      | None -> invalid_arg "the"
  in

    ODBContext.default ~logger 
      (the !dist_dir) 
      (the !incoming_dir)

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

let get_admin ~sp () = 
  get ~sp () 
  >>= fun ctxt -> 
    match ctxt.role with 
      | Account.Admin accnt ->
          return (ctxt, accnt)
      | Account.Anon | Account.User _ ->
          fail Common.RequiresAuth
