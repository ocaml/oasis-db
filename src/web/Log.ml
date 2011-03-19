
(** Log data 
    @author Sylvain Le Gall
  *)

open Lwt
open ODBGettext
open CalendarLib
open ODBLog

module S = Sqlexpr

let () = 
  S.register
    "log"
    2
    (fun db ->
       S.execute db 
         sqlinit"CREATE TABLE IF NOT EXISTS log\
          (id INTEGER PRIMARY KEY AUTOINCREMENT, \
           user_id INTEGER, \
           pkg TEXT, \
           ver TEXT, \
           sys TEXT, \
           event INTEGER NOT NULL, \
           sexp TEXT NOT NULL, \
           timestamp DATETIME DEFAULT (datetime('now')),
           CHECK ((pkg ISNULL AND sys NOTNULL) OR (pkg NOTNULL AND sys ISNULL)),
           CHECK (ver ISNULL OR pkg NOTNULL), 
           FOREIGN KEY(user_id) REFERENCES user(id))")
    (fun db v -> 
       match v with 
         | 1 ->
             S.execute db
               sql"ALTER TABLE log ADD COLUMN sexp TEXT NOT NULL"
         | _ ->
             return ())

let int_of_sevent = 
  function
    | `Undefined             -> 0
    | `Created               -> 1
    | `Deleted               -> 2
    | `Rated                 -> 3
    | `Commented             -> 4
    | `UscanChanged          -> 5
    | `Started               -> 6
    | `Stopped               -> 7
    | `VersionCreated        -> 8
    | `VersionDeleted        -> 9
    | `Message (`Fatal, _)   -> 10
    | `Message (`Error, _)   -> 11
    | `Message (`Warning, _) -> 12
    | `Message (`Notice, _)  -> 13
    | `Message (`Info, _)    -> 14
    | `Message (`Debug, _)   -> 15
    | `Failure               -> 16

(* TODO: remove
let sevent_of_int = 
  let max, assoc = 
    List.fold_left
      (fun (mx, assoc) ev -> 
         let i = 
           int_of_sevent ev
         in
           max mx i,
           (i, ev) :: assoc)
      (0, [])
      [`Created; 
       `Deleted; 
       `Rated; 
       `Commented; 
       `UscanChanged; 
       `Started; 
       `Stopped;
       `VersionCreated;
       `VersionDeleted;
       `Message `Fatal;
       `Message `Error;
       `Message `Warning;
       `Message `Notice;
       `Message `Info;
       `Message `Debug;
       `Failure;
      ]
  in
  let arr = 
    Array.make (max + 1) `Undefined
  in
  let () =
    List.iter 
      (fun (i, ev) ->
         arr.(i) <- ev)
      assoc
  in
    fun i -> arr.(i)
 *)

let add sqle ev =  
  S.use sqle
    (fun db ->
       let sexp = 
         Sexplib.Sexp.to_string 
           (ODBLog.sexp_of_event ev)
       in

       match ev with
         | `Pkg (pkg, se) ->
             S.execute db
               (sqlc"INSERT INTO log (pkg, event, sexp) VALUES (%s, %d, %s)")
               pkg (int_of_sevent se) sexp
         | `Sys (sys, se) ->
             S.execute db
               (sqlc"INSERT INTO log (sys, event, sexp) VALUES (%s, %d, %s)")
               sys (int_of_sevent se) sexp)

let get ?offset ?limit sqle =
  S.use sqle
    (fun db ->
       let decode acc (id, sexp, timestamp) = 
         id >>= fun id ->
         sexp >>= fun sexp ->
         timestamp >>= fun timestamp -> 
         begin
           let res = 
             {
               log_id = id;
               log_timestamp = 
                 (Printer.Calendar.from_string 
                    timestamp);
               log_event = 
                 (ODBLog.event_of_sexp 
                    (Sexplib.Sexp.of_string sexp));
             }
           in
             return (res :: acc)
         end
       in
       let exec sql = 
         S.fold db decode [] sql
       in
         (match offset, limit with 
            | None, None ->
                exec 
                  sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log ORDER BY timestamp DESC"
            | Some off, None ->
                exec 
                  sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log ORDER BY timestamp DESC OFFSET %d"
                  off
            | None, Some lmt ->
                exec 
                  (sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log ORDER BY timestamp DESC LIMIT %d")
                  lmt
            | Some off, Some lmt ->
                exec
                  (sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log ORDER BY timestamp DESC LIMIT %d OFFSET %d")
                  lmt off)
          >>= fun lst ->
          return (List.rev lst)) 

(* TODO: move to an appropriate place? *)

open Lwt_log

(** Return short symbol and CSS style *)
let html_log_level t =
  match t.log_event with 
    | `Sys (_, `Message (`Debug, _))    -> Some "log_debug" 
    | `Sys (_, `Message (`Info, _))     -> Some "log_info"
    | `Sys (_, `Message (`Notice, _))   -> Some "log_notice"
    | `Sys (_, `Message (`Warning, _))  -> Some "log_warning"
    | `Sys (_, `Message (`Error, _))    -> Some "log_error" 
    | `Sys (_, `Message (`Fatal, _))    -> Some "log_fatal"
    | `Sys (_, `Failure _)              -> Some "log_failure"
    | _ -> None 
