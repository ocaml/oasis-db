
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
      stor:  ODBVFS.read_write ODBStorage.read_write;
      sess:  OCASession.t option;
      accnt: OCAAccount.t option;
      rsync: (ODBVFS.read_write ODBSync.t) ref;
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

(* TODO: fix all this variables, it should just be used to build a 
 * default context 
 *)
let incoming_dir = mk_var "incoming directory"
let dist_dir     = mk_var "dist directory"
let sqle_fn      = mk_var "SQLite DB"

let ocaw         = mk_var "OCamlCore Web context"
let sqle         = mk_var "SQLExpr context"
let storage      = mk_var "Storage context"
let rsync        = mk_var "Synchronization context"

let google_analytics_account = ref None

let mkd_dir      = ref "src/web/mkd"

let admin        = ref []

let init_mutex   = Lwt_mutex.create () 
let init_val     = ref false

let () = 
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
    ODBContext.default ~logger (!incoming_dir ())

let init () = 

  let log ~timestamp ev = 
    Log.add ~timestamp (!sqle ()) ev
  in

  Lwt_mutex.with_lock init_mutex
    (fun () ->
       if not !init_val then 
         begin
           (* Initialize SQL context *)
           begin
             let log =
               ODBMessage.info ~ctxt:(get_odb ()) "%s"
             in
               Sqlexpr.create ~log (!sqle_fn ())
           end
           >>= fun sqle' ->
           return (sqle := (fun () -> sqle'))
           >>= fun () ->

           (* Initialize storage and only generate log event that are not already
            * in the DB 
            *)
           begin
             Log.get ~filter:(`Event `Created) (!sqle ())
             >>= fun pkg_created_evs ->
             Log.get ~filter:(`Event `VersionCreated) (!sqle ())
             >>= fun pkg_ver_created_evs ->
             begin 
               let fs :> ODBVFS.read_write = 
                 new ODBFSDiskLock.read_write
                   (new ODBFSDisk.read_write (!dist_dir ()))
                   (ODBRWLock.create ())
               in
               let () = 
                 ODBVFS.spy 
                   ~log:(fun msg -> 
                           log 
                             ~timestamp:(CalendarLib.Calendar.now ())
                             (`Sys ("VFS(dist)", `Message(`Debug, msg))))
                   fs
               in
                 ODBStorage.create
                   ~ctxt:(get_odb ()) 
                   fs
                   log
                   (List.rev_append pkg_created_evs pkg_ver_created_evs)
             end
           end
           >>= fun storage' ->
           return (storage := (fun () -> storage'))

           >>= fun () ->
           begin
             let dist_fs = ODBStorage.fs storage' in
               ODBSync.create ~ctxt:(get_odb ()) dist_fs
               >>= fun sync ->
               return (ref sync)
               >>= fun rsync ->
               begin
                 let () = 
                   ODBSync.autoupdate rsync
                 in
                   ODBSync.scan rsync
               end
               >>= fun () ->
               return rsync 
           end
           >>= fun rsync' ->
           return (rsync := (fun () -> rsync'))

           >|= fun () ->
           begin
             init_val := true;
             true
           end
         end
       else
         begin
           return false
         end)

  >>= fun first_init ->

  if first_init then
    (* Start incoming watch *)
    begin
      let _bkgrd_job = 
      (* TODO: get a semaphore to stop this background job *)
      ODBIncoming.start
        ~ctxt:(get_odb ()) 
        (!storage ())
        log
      in
        Log.add (!sqle ()) 
          (`Sys ("oasis-db web context", `Started))
    end
  else
    begin
      return ()
    end

let get_sys () =
  init ()
  >>= fun () ->
  begin
    let ocaw = 
      !ocaw ()
    in
      return 
        {
          odb   = get_odb ();
          sqle  = !sqle ();
          ocaw  = ocaw;
          mkd   = {Mkd.mkd_dir = !mkd_dir};
          stor  = (!storage ());
          sess  = None;
          accnt = None;
          admin = !admin;
          rsync = !rsync ();

          (* TODO: load configuration *)
          upload_delay = 5.0;
          upload_commit_delay = 5.0;
          upload_cancel_delay = 5.0;

          google_analytics_account = !google_analytics_account;
        }
  end

let get ~sp () = 
  get_sys ()
  >>= fun t ->
  OCAWeb.Session.get ~ctxt:t.ocaw sp
  >>= fun sess ->
  begin
    match sess with 
      | Some _ ->
          OCAWeb.Account.get ~sess ~ctxt:t.ocaw sp
      | None ->
          return None
  end
  >|= fun accnt ->
  {t with 
       sess = sess; 
       accnt = accnt;}


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
