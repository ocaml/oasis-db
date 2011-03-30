
open Lwt
open Lwt_log
open Simplexmlparser
open ODBGettext

type context =
    {
      odb:   ODBContext.t;
      sqle:  Sqlexpr.t;
      ocaw:  OCAWeb.t;
      mkd:   Mkd.t;
      sess:  OCASession.t option;
      accnt: OCAAccount.t option;
      admin: int list;

      upload_delay: float; 
      (* Delay for upload (wait for completion and refresh) *)   

      upload_commit_delay: float;
      (* Delay for an upload's commit (final step of upload) *)

      upload_cancel_delay: float;
      (* Delay to cancel an upload *)

      google_analytics_account: string option;
      (* Google Analytics account code *)
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
let ocaw         = mk_var "OCamlCore Web context"

let google_analytics_account = ref None

let mkd_dir      = ref "src/web/mkd"

let admin        = ref []

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
                | Element ("dir", ["rel", "mkd"], [PCData dn]) ->
                    mkd_dir := dn
                | Element ("db", [], [PCData fn]) ->
                    sqle_fn := fun () -> fn
                | Element ("user", ["role", "admin"], [PCData id_str]) ->
                    admin := (int_of_string id_str) :: !admin
                | Element ("ocamlcore-api", _, _) ->
                    (* Processed by OCAWeb.create_from_config *)
                    ()
                | Element ("google-analytics", ["account", code], []) ->
                    google_analytics_account := Some code

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

    (* Initialize OCamlCore API *)
    begin
      let vocaw = 
        OCAWeb.create_from_config 
          ~my_account:Common.my_account
          ~new_account:Common.new_account
          ()
      in
        ocaw := fun () -> vocaw;
    end;

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
  let () = 
    read_config ()
  in

  let log ~timestamp ev = 
    Log.add ~timestamp (!sqle ()) ev
  in

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
  >>= fun () ->
  return (Lwt_condition.broadcast init_cond ())
  >>= fun () ->

  (* Initialize storage and only generate log event that are not already
   * in the DB 
   *)
  Log.get ~filter:(`Event `Created) (!sqle ())
  >>= fun pkg_created_evs ->
  Log.get ~filter:(`Event `VersionCreated) (!sqle ())
  >>= fun pkg_ver_created_evs ->
  ODBStorage.start
    ~ctxt:(get_odb ()) 
    log
    (List.rev_append pkg_created_evs pkg_ver_created_evs)
  >>= fun () ->

  (* Start incoming watch *)
  begin
    let _bkgrd_job = 
    (* TODO: get a semaphore to stop this background job *)
    ODBIncoming.start
      ~ctxt:(get_odb ()) 
      log
    in
      return ()
  end 
  >>= fun () ->

  Log.add (!sqle ()) 
    (`Sys ("oasis-db web context", `Started))


let get ~sp () = 
  Lwt_mutex.with_lock init_mutex
    (fun () ->
       begin
         if !init_val then
           return ()
         else
           Lwt_condition.wait ~mutex:init_mutex init_cond
       end)
  >>= fun () ->
  begin
    let ocaw = 
      !ocaw ()
    in
      OCAWeb.Session.get ~ctxt:ocaw sp
      >>= fun sess ->
      begin
        match sess with 
          | Some _ ->
              OCAWeb.Account.get ~sess ~ctxt:ocaw sp
          | None ->
              return None
      end
      >>= fun accnt ->
      return 
        {
          odb   = get_odb ();
          sqle  = !sqle ();
          ocaw  = ocaw;
          mkd   = {Mkd.mkd_dir = !mkd_dir};
          sess  = sess;
          accnt = accnt;
          admin = !admin;

          (* TODO: load configuration *)
          upload_delay = 5.0;
          upload_commit_delay = 5.0;
          upload_cancel_delay = 5.0;

          google_analytics_account = !google_analytics_account;
        }
  end


let is_anon ~ctxt () = 
  ctxt.accnt = None

let is_user ~ctxt () = 
  ctxt.accnt <> None

let is_admin ~ctxt ?accnt () = 
  let accnt =
    match accnt with
      | Some accnt -> accnt
      | None -> ctxt.accnt
  in
    match accnt with
      | Some accnt when
          List.mem accnt.OCAAccount.accnt_id ctxt.admin ->
          true
      | _ ->
          false

let get_user ~sp () = 
  get ~sp () 
  >>= fun ctxt -> 
    match ctxt.accnt with 
      | Some accnt ->
          return (ctxt, accnt)
      | None ->
          fail Common.RequiresAuth

let get_admin ~sp () = 
  get_user ~sp () 
  >>= fun (ctxt, accnt) -> 
    if is_admin ~ctxt () then 
      return (ctxt, accnt)
    else
      fail Common.RequiresAuth

(* Make a context belong to anon *)
let anonymize ctxt =
  {ctxt with accnt = None; sess = None}
