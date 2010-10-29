
(** Long running tasks 
  *)

open ODBContext
open Context
open Lwt
open Lwt_log

type 'a t =
    {
      creator:    Account.t;
      start_time: float;
      thrd:       ('a * float) Lwt.t;
      log:        (section * level * string) Queue.t;
    }

type id = int

exception NoTask of id

let create () = 
  Hashtbl.create 5

let last_id = 
  ref 0


let add t f ~ctxt e = 
  let log = 
    Queue.create ()
  in
  let queue_logger = 
    Lwt_log.make
      ~output:(fun sct lvl lst ->
                 List.iter 
                   (fun str -> Queue.add (sct, lvl, str) log)
                   lst;
                 return ())
      ~close:(fun () -> return ())
  in
  let ctxt = 
    (* Override logger to get data from it *)
    {ctxt with 
         odb = 
           {ctxt.odb with 
                logger = 
                  Lwt_log.broadcast 
                    [queue_logger; ctxt.odb.logger]}}
  in
  let thrd =
    f ~ctxt e
    >|= fun res ->
    res, Unix.gettimeofday ()
  in
  let e =
    {
      start_time = Unix.gettimeofday ();
      creator    = ctxt.role;
      thrd       = thrd;
      log        = log;
    }
  in
    incr last_id;
    Hashtbl.add t !last_id e;
    !last_id

let wait t ~ctxt id timeout timeout_msg =
  try 
    let t = 
      Hashtbl.find t id 
    in
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
      >|= fun (e, end_time) -> 
      t,
      end_time -. t.start_time, (* delay *)
      e (* result *)

  with Not_found ->
    fail (NoTask id)

(* TODO: consider moving this to Log *)

(** Share the log queue with another context *)
let set_logger t ctxt = 
  let queue_logger = 
    Lwt_log.make
      ~output:(fun sct lvl lst ->
                 List.iter 
                   (fun str -> Queue.add (sct, lvl, str) t.log)
                   lst;
                 return ())
      ~close:(fun () -> return ())
  in
    (* Override logger to get data from it *)
    {ctxt with 
         odb = 
           {ctxt.odb with 
                logger = 
                  Lwt_log.broadcast 
                    [queue_logger; ctxt.odb.logger]}}

(** Get the log queue of this task *)
let get_logger t =
  Queue.copy t.log
