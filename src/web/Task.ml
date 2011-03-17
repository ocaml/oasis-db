
(** Long running tasks 
  *)

open ODBContext
open Context
open Lwt
open Lwt_log

type ('a, 'b) t =
    {
      creator:    OCAAccount.t option;
      start_time: float;
      thrd:       ('b * float) Lwt.t;
      log:        LogBox.t;
    }

let create ~ctxt f = 
  let log = 
    LogBox.create ()
  in
  let ctxt = 
    LogBox.set log ctxt
  in
  let thrd =
    f ctxt 
    >|= fun res ->
    res, Unix.gettimeofday ()
  in
    {
      start_time = Unix.gettimeofday ();
      creator    = ctxt.accnt;
      thrd       = thrd;
      log        = log;
    }

let wait ~ctxt t timeout timeout_msg =
  (* Try to get result from the thread
   * or return None
   *)
  let timeout_thrd = 
    Lwt_unix.timeout timeout
  in

    catch 
      (fun () ->
         choose [timeout_thrd; t.thrd]
         >|= fun res -> 
         Lwt.cancel timeout_thrd;
         res)
      (function
         | Lwt_unix.Timeout ->
             fail (Common.Timeout timeout_msg)
         | e ->
             fail e)
    >|= fun (res, end_time) -> 
    t,
    end_time -. t.start_time, (* delay *)
    res (* result *)

let create_and_wait ~ctxt f set timeout timeout_msg = 
  let t = 
    create f ~ctxt 
  in
    set t;
    wait ~ctxt t timeout timeout_msg 

(** Share the log queue with another context *)
let set_logger t ctxt = 
  LogBox.set t.log ctxt

(** Get the log queue of this task *)
let get_logger t =
  LogBox.copy t.log
