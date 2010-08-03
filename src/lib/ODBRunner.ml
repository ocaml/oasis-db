

open Lwt 
open ODBGettext
open ODBMessage

(** Allow only one loop at a time
 *)
let singleton nm f = 
  let mtx =
    Lwt_mutex.create ()
  in

    fun ~ctxt () ->
      if Lwt_mutex.is_locked mtx then 
        begin
          fail 
            (Failure 
              (Printf.sprintf
                (f_ "Try to run singleton function %s another time")
                nm))
        end
      else 
        begin
          let rec restart () = 
            let start_time = 
              Unix.gettimeofday ()
            in
              catch 
                (f ~ctxt)
                (function
                   | e ->
                       let running_time = 
                         (Unix.gettimeofday ()) -. start_time
                       in
                         error ~ctxt
                           (f_ "Function %s ends with exception after \
                                %.2f seconds: %s")
                           nm
                           running_time
                           (string_of_exception e)
                         >>= fun () ->
                         if running_time < ODBConf.min_running_time then 
                           begin
                             let delayed =
                               ODBConf.min_running_time -. running_time
                             in
                               warning ~ctxt
                                 (f_ "Function %s doesn't run for enough time, \
                                      restart delayed for %.2f seconds")
                                 nm
                                 delayed
                              >>= fun () ->
                              Lwt_unix.sleep delayed
                              >>=
                              restart
                           end
                         else
                           begin
                             restart ()
                           end)
          in
            Lwt_mutex.with_lock mtx restart             
        end
