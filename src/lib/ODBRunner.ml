

open Lwt 
open ODBGettext

(** Allow only one loop at a time
 *)
let singleton nm f = 
  let mtx =
    Lwt_mutex.create ()
  in

    fun ~ctxt () ->
      if Lwt_mutex.is_locked mtx then 
        fail 
          (Failure 
            (Printf.sprintf
              (f_ "Try to run singleton function %s another time")
              nm))
      else 
        Lwt_mutex.with_lock mtx (f ~ctxt)
