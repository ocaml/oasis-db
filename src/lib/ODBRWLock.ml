
(** Reader-writer lock 

    Synchronization primitive that allow multiple readers but a single writer.

    @author Sylvain Le Gall
  *)

open Lwt

type t = unit

let create () = 
  ()

let read_lock t =
  return ()

let read_unlock t =
  return ()

let write_lock t =
  return ()

let write_unlock t =
  return ()

let with_read_lock t f =
  read_lock t >>= f >>= fun res -> read_unlock t >|= fun () -> res

let with_write_lock t f =
  write_lock t >>= f >>= fun res -> write_unlock t >|= fun () -> res
