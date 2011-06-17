
(** Manage the DB and its creation/upgrade
    @author Sylvain Le Gall
  *)

open Lwt 

module Sqlexpr = 
  Sqlexpr_sqlite.Make
    (struct 
       include Lwt 
       let auto_yield = Lwt_unix.auto_yield
       let sleep = Lwt_unix.sleep
     end)

include Sqlexpr

module S = Sqlexpr

type t = Sqlexpr_sqlite.db Lwt_pool.t

let fun_init = 
  ref [] 

let register key ver init upgrade =
  fun_init := (key, ver, init, upgrade) :: !fun_init

let use t f = 
  Lwt_pool.use t f

let create ?(log) ?(n=5) fn = 
   let log fmt =
     let output = 
       match log with 
         | Some f ->
             fun s ->
               f s
         | None ->
             fun s ->
               Lwt_io.eprintl s
     in
       Printf.ksprintf output fmt
   in

   let res = 
     Lwt_pool.create n (fun () -> return (open_db fn))
   in

   let init db = 
     execute db
       sqlinit"CREATE TABLE IF NOT EXISTS version \
        (key TEXT PRIMARY KEY NOT NULL,
         ver INTEGER NOT NULL)"
     >>= fun () ->
     Lwt_list.iter_s
       (fun (key, latest_ver, init, upgrade) ->
          catch 
            (fun () ->
               select_one db
                 sql"SELECT @d{ver} FROM version WHERE key = %s"
                 key
               >>= fun ver ->

                 if ver = latest_ver then
                   log 
                     "Table(s) %s already at latest version (%d)"
                     key ver
                 else
                   log
                     "Upgrading table(s) %s from version %d to %d"
                     key ver latest_ver
                   >>= fun () ->
                   upgrade db ver
                   >>= fun () ->
                   execute db
                     sql"UPDATE version SET ver = %d WHERE key = %s"
                     latest_ver key)

            (function 
               | Not_found ->
                   log
                     "Creating table(s) %s at version %d"
                     key latest_ver
                   >>= fun () ->
                   init db 
                   >>= fun () ->
                   execute db 
                     sql"INSERT INTO version (key, ver) VALUES (%s, %d)"
                     key latest_ver
               | e ->
                   fail e))
       (List.rev !fun_init)
   in

     use res init
     >>= fun () ->
     return res

let () = 
  Printexc.register_printer
    (function
       | Sqlexpr_sqlite.Error(e) ->
           Some "Sqlexpr_sqlite.Error(...)" 
(*
             (Printf.sprintf "Sqlexpr_sqlite.Error(%s)"
                (Printexc.to_string e))
 *)
       | _ ->
           None)
