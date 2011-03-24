
(** Log data 
    @author Sylvain Le Gall
  *)

open Lwt
open ODBGettext
open CalendarLib
open ODBLog
open ODBUtils

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

type sevent =
  [ `Created
  | `Deleted
  | `Rated
  | `Commented
  | `UscanChanged
  | `Started
  | `Stopped
  | `VersionCreated
  | `VersionDeleted
  | `Message of sys_event_level
  | `Failure
  ]

let int_of_sevent = 
  function
    | `Created               -> 1
    | `Deleted               -> 2
    | `Rated                 -> 3
    | `Commented             -> 4
    | `UscanChanged          -> 5
    | `Started               -> 6
    | `Stopped               -> 7
    | `VersionCreated        -> 8
    | `VersionDeleted        -> 9
    | `Message `Fatal        -> 10
    | `Message `Error        -> 11
    | `Message `Warning      -> 12
    | `Message `Notice       -> 13
    | `Message `Info         -> 14
    | `Message `Debug        -> 15
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

let add sqle ?timestamp (ev: ODBLog.event) =  
  let sevent_of_xxx_event = 
    (* Adapt event from ODBLog to sevent *)
    function
      | `VersionCreated _ -> `VersionCreated
      | `VersionDeleted _ -> `VersionDeleted
      | `Failure _        -> `Failure
      | `Message (lvl, _) -> `Message lvl
      | `Created
      | `Deleted
      | `UscanChanged
      | `Rated
      | `Commented
      | `Started
      | `Stopped as e ->
          e
  in
    S.use sqle
      (fun db ->
         let sexp = 
           Sexplib.Sexp.to_string 
             (ODBLog.sexp_of_event ev)
         in

         let sys_opt, pkg_opt, ver_opt, se =
           match ev with
             | `Pkg (pkg, se) ->
                 begin
                   let ver_opt = 
                     match se with 
                       | `VersionCreated ver 
                       | `VersionDeleted ver ->
                           Some (OASISVersion.string_of_version ver)
                       | `Created | `Deleted | `UscanChanged
                       | `Rated | `Commented ->
                           None
                   in
                     None, (Some pkg), ver_opt, 
                     (sevent_of_xxx_event se)
                 end

             | `Sys (sys, se) ->
                 (Some sys), None, None, 
                 (sevent_of_xxx_event se)
         in

           
         let exec =
           match timestamp with 
             | Some tm ->
                 S.execute db
                   (sql"INSERT INTO log (timestamp, sys, pkg, ver, event, sexp) \
                        VALUES (%s, %s?, %s?, %s?, %d, %s)")
                   (Printer.Calendar.to_string tm)
             | None ->
                 S.execute db
                   (sql"INSERT INTO log (sys, pkg, ver, event, sexp) \
                        VALUES (%s?, %s?, %s?, %d, %s)")
         in
           exec sys_opt pkg_opt ver_opt (int_of_sevent se) sexp)

type filter =
    [ `Pkg of string
    | `Sys of string 
    | `Event of event 
    ]

let exec_fold_decode db sql =
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
   S.fold db decode [] sql

let get ?(offset=0) ?(limit=(-1)) ?filter sqle =
  S.use sqle
    (fun db ->
       begin
         match filter with 
           | None ->
               exec_fold_decode db
                 (sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log \
                      ORDER BY timestamp DESC LIMIT %d OFFSET %d")
                 limit offset

           | Some (`Pkg pkg_str) ->
               exec_fold_decode db
                 (sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log \
                      WHERE pkg = %s \
                      ORDER BY timestamp DESC LIMIT %d OFFSET %d")
                 pkg_str
                 limit offset

           | Some (`Sys sys_str) ->
               exec_fold_decode db
                 (sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log \
                      WHERE sys = %s \
                      ORDER BY timestamp DESC LIMIT %d OFFSET %d")
                 sys_str
                 limit offset

           | Some (`Event sev) ->
               exec_fold_decode db
                 (sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log \
                      WHERE event = %d \
                      ORDER BY timestamp DESC LIMIT %d OFFSET %d")
                 (int_of_sevent sev)
                 limit offset
       end
       >>= fun lst ->
       return (List.rev lst))

let get_rev ?filter ?(limit=(-1)) ?(offset=0) sqle = 
  (* TODO: code duplicate *)
  S.use sqle
    (fun db ->
       begin
         match filter with 
           | None ->
               exec_fold_decode db
                 (sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log \
                      ORDER BY timestamp ASC LIMIT %d OFFSET %d")
                 limit offset

           | Some (`Pkg pkg_str) ->
               exec_fold_decode db
                 (sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log \
                      WHERE pkg = %s \
                      ORDER BY timestamp ASC LIMIT %d OFFSET %d")
                 pkg_str
                 limit offset

           | Some (`Sys sys_str) ->
               exec_fold_decode db
                 (sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log \
                      WHERE sys = %s \
                      ORDER BY timestamp ASC LIMIT %d OFFSET %d")
                 sys_str
                 limit offset

           | Some (`Event sev) ->
               exec_fold_decode db
                 (sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log \
                      WHERE event = %d \
                      ORDER BY timestamp ASC LIMIT %d OFFSET %d")
                 (int_of_sevent sev)
                 limit offset
       end
       >>= fun lst ->
       return (List.rev lst))

let get_count ?filter sqle =
  S.use sqle
    (fun db ->
       begin
         match filter with 
           | None ->
               S.select_one db
                 (sql"SELECT @d{count(*)} FROM log")

           | Some (`Pkg pkg_str) ->
               S.select_one db
                 (sql"SELECT @d{count(*)} FROM log WHERE pkg = %s")
                 pkg_str

           | Some (`Sys sys_str) ->
               S.select_one db
                 (sql"SELECT @d{count(*)} FROM log WHERE sys = %s")
                 sys_str

           | Some (`Event sev) ->
               S.select_one db
                 (sql"SELECT @d{count(*)} FROM log WHERE event = %d")
                 (int_of_sevent sev)
       end
       >>= fun count ->
       return count)

let upload_stats sqle fst_date = 
  let fold_decode ev = 
    S.use sqle
      (fun db ->
         S.fold db
           (fun acc (year_str, month_str, count) ->
              year_str >>= fun year_str ->
              month_str >>= fun month_str ->
              count >>= fun count ->
              return 
                ((Calendar.lmake
                    ~year:(int_of_string year_str)
                    ~month:(int_of_string month_str)
                    (),
                  count) 
                :: acc))
           []
           sql"SELECT @s{strftime('%%Y', timestamp) AS year}, \
                      @s{strftime('%%m', timestamp) AS month}, \
                      @d{count(*)} FROM log \
               WHERE pkg NOT NULL AND event = %d AND timestamp >= %f \
               GROUP BY year, month ORDER BY year DESC, month DESC"
           (int_of_sevent ev)
           (Calendar.to_unixfloat fst_date))
  in
    fold_decode `VersionCreated
    >>= fun pkg_ver_created ->
    fold_decode `Created
    >>= fun pkg_created ->
    fold_decode `Deleted
    >>= fun pkg_deleted ->
    begin
      let comp_yearmonth c1 c2 = 
        match (Calendar.year c1) - (Calendar.year c2) with 
          | 0 ->
              compare (Calendar.month c1) (Calendar.month c2)
          | n ->
              n
      in

      let lst_date =
        let rec lst_date' = 
          function 
            | [] ->
                lst_date' [Calendar.now ()]
            | hd :: _ as lst ->
                let nhd = 
                  match Calendar.year hd, Calendar.month hd with 
                    | year, Calendar.Jan ->
                        Calendar.lmake ~year:(year - 1) ~month:12 ()
                    | year, _ ->
                        Calendar.lmake 
                          ~year
                          ~month:(let imonth = 
                                    Date.int_of_month 
                                      (Calendar.month hd)
                                  in
                                    imonth - 1)
                          ()
                in

                let cmp = 
                  comp_yearmonth nhd fst_date 
                in
                  if cmp = 0 then
                    nhd :: lst
                  else if cmp < 0 then 
                    lst
                  else
                    lst_date' (nhd :: lst)
        in
          lst_date' []
      in

      let rec combine num_pkg lst_date pkg_ver_created pkg_created pkg_deleted =
        let pick_match date lst f dflt = 
          match lst with 
            | (date', v) :: tl ->
                if comp_yearmonth date date' = 0 then
                  (f v), tl
                else
                  dflt, lst
            | [] ->
                dflt, lst
        in

          match lst_date with 
            | cur_date :: tl_date ->
                begin
                  let num_pkg, pkg_created = 
                    pick_match 
                      cur_date 
                      pkg_created 
                      (fun v -> num_pkg + v)
                      num_pkg
                  in
                  let num_pkg, pkg_deleted =
                    pick_match
                      cur_date
                      pkg_deleted
                      (fun v -> num_pkg - v)
                      num_pkg
                  in
                  let uploads, pkg_ver_created =
                    pick_match 
                      cur_date
                      pkg_ver_created
                      (fun v -> v)
                      0                      
                  in
                    (cur_date, num_pkg, uploads) :: 
                    (combine 
                       num_pkg 
                       tl_date 
                       pkg_ver_created 
                       pkg_created 
                       pkg_deleted)
                end

            | [] ->
                (* TODO: warning if all lists are not empty *)
                []
      in

      let res =
        combine 0 lst_date pkg_ver_created pkg_created pkg_deleted
      in
        return res
    end


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
