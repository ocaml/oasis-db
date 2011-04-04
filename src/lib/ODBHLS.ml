
(** A container that mix a Hashtbl with a way to fast 
    enumerate elements (based on Queue)
  *)

open Lwt

let debug fmt =
  Printf.ksprintf 
    (fun s ->
       if false then
         Lwt_io.eprintl s
       else
         return ())
    fmt

module HashString = 
  Hashtbl.Make
    (struct
       type t = string
       let equal (s1: string) s2 = s1 = s2
       let hash = Hashtbl.hash
     end)

type 'a t = 
    {
      tbl:  'a HashString.t;
      que:   string Queue.t;
      rwlck: ODBRWLock.t;
    }

let create () = 
  {
    tbl   = HashString.create 13;
    que   = Queue.create ();
    rwlck = ODBRWLock.create ();
  }

let add t k v = 
  ODBRWLock.with_write_lock t.rwlck
    (fun () ->
       debug "ODBHLS.add %s" k
       >>= fun () ->
       begin
         if not (HashString.mem t.tbl k) then
           Queue.add k t.que;
         HashString.replace t.tbl k v;
         return ()
       end)

let find t k =
  ODBRWLock.with_read_lock t.rwlck
    (fun () ->
       try 
         debug "ODBHLS.find %s" k
         >>= fun () ->
         return (HashString.find t.tbl k)
       with e ->
         fail e)

let mem t k =
  ODBRWLock.with_read_lock t.rwlck
    (fun () ->
       debug "ODBHLS.mem %s" k
       >>= fun () ->
       return (HashString.mem t.tbl k))

let elements t = 
  ODBRWLock.with_read_lock t.rwlck
    (fun () ->
       debug "ODBHLS.elements" 
       >>= fun () ->
       let lst = 
         Queue.fold 
           (fun acc k ->
              (k, HashString.find t.tbl k) :: acc)
           []
           t.que
       in
         return lst)
