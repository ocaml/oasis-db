
(** Reader-writer lock 

    Synchronization primitive that allow multiple readers but a single writer.

    @author Sylvain Le Gall
  *)

type t = unit

let create () = 
  ()

let with_read_lock t f =
  f ()

let with_write_lock t f =
  f ()
