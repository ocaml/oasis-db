
(* PGOCaml interface with Lwt *)

module PGOCaml = 
  PGOCaml_generic.Make 
    (struct 
       include Lwt 
       include Lwt_chan 
     end)

let () = 
  PGOCaml.verbose := 2

open Lwt

type db_t = (string, bool) Hashtbl.t PGOCaml.t

let connect () =
    PGOCaml.connect ()

let pool = Lwt_pool.create 16 connect

let transaction_block db f =
  PGOCaml.begin_work db >>= fun _ ->
    Lwt.catch
      (fun () ->
         f () >>= fun r ->
           PGOCaml.commit db >>= fun () ->
             Lwt.return r)
      (fun e ->
         PGOCaml.rollback db >>= fun () ->
           Lwt.fail e)

let full_transaction_block f =
    Lwt_pool.use pool (fun db -> transaction_block db (fun () -> f db))
