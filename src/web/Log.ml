
(** Log data 
    @author Sylvain Le Gall
  *)

open Lwt
open Lwt_log
open Sexplib
open SQL
open ODBGettext

TYPE_CONV_PATH "Log"

type what =
  | Package  of string
  | Version  of string * string
  | Incoming of string (* tarball *)
  | Auth
  | Other with sexp  

type replay =  
    (* Look like a function *)
    string * string list with sexp 

type t = 
    {
      id:      int32;
      level:   Lwt_log.level;
      what:    what;
      path:    string list;
      message: string;
      replay:  replay;
    }

let string_of_level = 
  [
    Error,   "E";
    Fatal,   "F";
    Warning, "W";
    Notice,  "N";
    Info,    "I";
    Debug,   "D";
  ]

let level_of_string = 
  List.map (fun (a, b) -> (b, a)) string_of_level

let add level what path replay messages = 
  let pglvl  = List.assoc level string_of_level in
  let pgwhat = Sexp.to_string (sexp_of_what what) in
  let pgreplay = Sexp.to_string (sexp_of_replay replay) in
    
    Lwt_pool.use SQL.pool
      (fun db ->
         Lwt_list.iter_s
           (fun msg -> 
              PGSQL(db)
                "INSERT INTO log (level, what, path, message, replay) \
                 VALUES ($pglvl, $pgwhat, $path, $msg, $pgreplay)")
           messages)


type get_what = 
  | All 
  | Only of what list with sexp 

type order =
  | LevelAndDate
  | DateAndLevel with sexp 

let get what order offset limit = 
  Lwt_pool.use SQL.pool
    (fun db ->
       PGSQL(db)  
         "SELECT id, level, what, message, replay, timestamp FROM log \
          ORDER BY timestamp DESC OFFSET $offset LIMIT $limit"
        >>= fun lst ->
        return 
          (List.map
             (function (id, pglvl, pgwhat, msg, pgreplay, timestamp) ->
               fst timestamp,
               snd timestamp,
               List.assoc pglvl level_of_string,
               msg,
               "")
             lst))


(** Return short symbol and CSS style *)
let html_log_level =
  function
    | Debug    -> s_ "D", "log_debug" 
    | Info     -> s_ "I", "log_info"
    | Notice   -> s_ "N", "log_notice"
    | Warning  -> s_ "W", "log_warning"
    | Error    -> s_ "E", "log_error" 
    | Fatal    -> s_ "F", "log_fatal"
