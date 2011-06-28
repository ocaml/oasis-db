
(** Web services to manage monitored packages
    @author Sylvain Le Gall
  *)

open Lwt
open XHTML.M
open ODBGettext
open Common
open Eliom_services
open Context
open Eliom_parameters
open Eliom_predefmod.Xhtml
open CalendarLib
open Feeds


module S = Sqlexpr

let () = 
  let create_token_table db = 
    S.execute db
      sqlinit"CREATE TABLE IF NOT EXISTS monitor_token\
       (token STRING NOT NULL,
        user_id INTEGER NOT NULL)"
  in
    S.register 
      "monitor"
      2
      (fun db ->
         S.execute db
           sqlinit"CREATE TABLE IF NOT EXISTS monitor_pkg\
            (id INTEGER PRIMARY KEY AUTOINCREMENT, \
             user_id INTEGER NOT NULL,
             pkg TEXT NOT NULL)"
         >>= fun () ->
         create_token_table db)
      (fun db ver ->
         match ver with 
           | 1 -> 
               create_token_table db
           | _ ->
               return ())

let monitor_pkg_list_settings =
  Eliom_sessions.create_volatile_table () 

let user_settings_box ~sp ~ctxt () = 
  let unmonitor_list = 
    Eliom_predefmod.Action.register_new_post_coservice_for_session'
      ~sp
      ~name:"unmonitor_pkg_lst"
      ~post_params:(set string "pkg")
      (fun sp () pkg_lst ->
         Context.get_user ~sp () 
         >>= fun (ctxt, accnt) ->
         S.use ctxt.sqle
           (fun db ->
              Lwt_list.iter_s
                (fun pkg -> 
                   S.execute db
                     sql"DELETE FROM monitor_pkg WHERE user_id = %d AND pkg = %s"
                     accnt.OCAAccount.accnt_id pkg)
                pkg_lst))
  in

  let monitor_list = 
    Eliom_predefmod.Action.register_new_post_coservice_for_session'
      ~sp
      ~name:"monitor_pkg_lst"
      ~post_params:(set string "pkg")
      (fun sp () pkg_lst ->
         Context.get_user ~sp ()
         >>= fun (ctxt, accnt) ->
         S.use ctxt.sqle
           (fun db ->
              Lwt_list.iter_s
                (fun pkg ->
                   S.execute db
                     sql"INSERT INTO monitor_pkg(user_id, pkg) VALUES (%d, %s)"
                     accnt.OCAAccount.accnt_id pkg)
                pkg_lst))
  in

  let switch_pkg_list =
    Eliom_predefmod.Action.register_new_post_coservice_for_session'
      ~sp
      ~name:"monitor_switch_pkg_list"
      ~post_params:(string "show")
      (fun sp () show ->
         Context.get_user ~sp ()
         >|= fun (ctxt, accnt) ->
         Eliom_sessions.set_volatile_session_data 
           ~table:monitor_pkg_list_settings
           ~sp 
           (bool_of_string show))
  in

  let show_pkg_list = 
    match Eliom_sessions.get_volatile_session_data
            ~table:monitor_pkg_list_settings
            ~sp () with 
      | Eliom_sessions.Data show -> show
      | Eliom_sessions.Data_session_expired
      | Eliom_sessions.No_data ->
          false
  in

  let mk_pkg_select_table nm lst =
    ncol_table 
      3
      (tr (td [pcdata "\030"]) [])
      (List.map 
         (fun pkg_str ->
            (tr 
               (td
                  [
                    string_checkbox ~name:nm ~value:pkg_str ();
                    a view_pkg_ver sp [pcdata pkg_str] (pkg_str, LatestVersion)
                  ])
               []))
         (List.sort String.compare lst))
  in
         
  ODBStorage.Pkg.elements ctxt.stor 
  >>= fun other_pkg_lst ->
  S.use ctxt.sqle
    (fun db ->
       begin
         match Account.get_id ~ctxt () with 
           | Some id ->
               S.select db
                 sql"SELECT @s{pkg} FROM monitor_pkg WHERE user_id = %d"
                 id
           | None ->
               return []
       end
       >>= fun lst ->
       Lwt_list.fold_left_s 
         (fun acc pkg_str ->
            ODBStorage.Pkg.mem ctxt.stor (`Str pkg_str)
            >|= fun exists ->
              if exists then
                pkg_str :: acc
              else
                acc)
         []
         lst)
  >>= fun pkg_str_lst ->
  let other_pkg_lst = 
    List.filter 
      (fun str -> not (List.mem str pkg_str_lst))
      (List.rev_map 
         (fun pkg -> pkg.ODBPkg.pkg_name)
         other_pkg_lst)
  in
  let show_pkg_list = 
    if other_pkg_lst = [] then
      false
    else
      show_pkg_list
  in
    return 
      (div 
         [h3 [pcdata (s_ "Monitor")];
          post_form 
            ~service:unmonitor_list
            ~sp
            (fun pkg_nm ->
               [
                 if pkg_str_lst = [] then
                   p [pcdata (s_ "No packages monitored")]
                 else
                   mk_pkg_select_table pkg_nm pkg_str_lst;

                 p [string_input 
                      ~a:(if pkg_str_lst = [] then
                            [a_disabled `Disabled]
                          else
                            [])
                      ~input_type:`Submit 
                      ~value:(s_ "Stop monitor selected") ()];
               ])
            ();
          post_form
            ~service:switch_pkg_list
            ~sp 
            (fun switch_nm ->
               [p 
                  [string_button
                     ~a:(if other_pkg_lst = [] then
                           [a_disabled `Disabled]
                         else
                           [])
                     ~value:(string_of_bool (not show_pkg_list))
                     ~name:switch_nm
                     [pcdata
                        (if show_pkg_list then
                           s_ "Hide other packages"
                         else
                           s_ "Show other packages")]]])
            ();

          if show_pkg_list then 
            begin
              post_form
                ~service:monitor_list
                ~sp 
                (fun pkg_nm ->
                   [
                     mk_pkg_select_table pkg_nm other_pkg_lst;
                     p [string_input ~input_type:`Submit ~value:(s_ "Start monitor selected") ()];
                   ])
                ();
            end
          else
            pcdata ""
         ])

let monitor = 
  Eliom_predefmod.Action.register_new_coservice'
    ~name:"monitor_pkg"
    ~get_params:(ExtParams.pkg "pkg" ** bool "monitor")
    (fun sp (k, monitor) () ->
       Context.get_user ~sp () 
       >>= fun (ctxt, _) ->
       match Account.get_id ~ctxt () with 
         | Some id ->
             S.use ctxt.sqle
               (fun db ->
                  S.execute db 
                  (if monitor then
                     sql"INSERT INTO monitor_pkg(user_id, pkg) VALUES (%d, %s)"
                   else
                     sql"DELETE FROM monitor_pkg WHERE user_id = %d AND pkg = %s")
                    id (ExtParams.pkg_str_of_pkg_k k))
         | None ->
             return ())

let box ~sp ~ctxt k = 
  S.use ctxt.sqle
    (fun db ->
       begin
         match Account.get_id ~ctxt () with 
           | Some id ->
               S.select_one db
                 sql"SELECT @d?{count(*)} FROM monitor_pkg WHERE pkg = %s AND user_id = %d"
                 (ExtParams.pkg_str_of_pkg_k k) id
           | None ->
               return None
       end
       >|= fun mark_opt ->
         let vmonitor, txt =
           match mark_opt with 
             | Some 0 
             | None -> 
                 true, s_ "Start monitor"
             | Some _ -> 
                 false, s_ "Stop monitor"
         in
           (Session.link_need_login ~sp ~ctxt
              txt
              (preapply monitor (k, vmonitor))))

let monitor_data ~ctxt limit offset = 
  match Account.get_id ~ctxt (), Context.is_admin ~ctxt () with 
    | Some id, false ->
        S.use ctxt.sqle
          (fun db ->
             Log.exec_fold_decode db
               (sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log \
                    WHERE pkg IN (SELECT pkg FROM monitor_pkg WHERE user_id = %d) \
                    ORDER BY timestamp DESC LIMIT %d OFFSET %d")
               id limit offset)
    | Some id, true ->
        S.use ctxt.sqle
          (fun db ->
             Log.exec_fold_decode db
               (sql"SELECT @d{id}, @s{sexp}, @s{timestamp} FROM log \
                    WHERE pkg IN (SELECT pkg FROM monitor_pkg WHERE user_id = %d) OR
                          sys NOT NULL \
                    ORDER BY timestamp DESC LIMIT %d OFFSET %d")
               id limit offset)
    | None, _ ->
        return []

let monitor_token_allocate ~ctxt () = 
  (* Test existence of the token and inject it if possible *)
  match Account.get_id ~ctxt () with 
    | Some id ->
        S.use ctxt.sqle
          (fun db ->
             S.transaction db
               (fun db ->
                  let rec allocate' () =
                    let token = 
                      string_of_int (Random.bits ()) 
                    in
                      S.select_one db
                        sql"SELECT @d{count(token)} FROM monitor_token WHERE token = %s"
                        token
                      >>=
                        function 
                          | 0 ->
                              begin
                                S.execute db
                                  sql"INSERT INTO monitor_token(user_id, token) VALUES (%d, %s)"
                                  id token
                                >|= fun () ->
                                Some token
                              end

                          | _ ->
                              allocate' ()
                  in
                    S.execute db
                      sql"DELETE FROM monitor_token WHERE user_id = %d"
                      id
                    >>= fun () ->
                    allocate' ()))
    | None ->
        return None

let monitor_token ~ctxt () = 
  match Account.get_id ~ctxt () with 
    | Some id ->
        begin
          S.use ctxt.sqle
            (fun db ->
               S.select db
                 sql"SELECT @s{token} FROM monitor_token WHERE user_id = %d"
                 id)
          >>= 
            function
              | token :: _ ->
                  return (Some token)
              | [] -> 
                  begin
                    (* We need to allocate a token *)
                    monitor_token_allocate ~ctxt ()
                  end
        end

     | None ->
         return None

let token_realloc =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"monitor_token_realloc"
    ~post_params:unit
    (fun sp () () ->
       Context.get_user ~sp ()
       >>= fun (ctxt, _) ->
       monitor_token_allocate ~ctxt () 
       >>= 
         function 
           | Some _ ->
               return ()
           | None ->
               fail 
                 (Failure (s_ "Unable to create a new token")))

let feed = 
  new_service 
    ~path:["feeds"; "private"; "rss2"] 
    ~get_params:(string "token")
     ()

let feed_handler sp token ()  = 
  Context.get ~sp () 
  >>= fun ctxt ->
  (* Get account matching the provided token *)
  S.use ctxt.sqle 
    (fun db ->
       S.select_one db
         sql"SELECT @d{user_id} FROM monitor_token WHERE token = %s"
         token)
  >>= fun user_id ->
  Account.of_id ~ctxt user_id
  >>= fun accnt ->
  begin
    let ctxt = 
      {ctxt with accnt = Some accnt}
    in
      monitor_data ~ctxt 40 0
      >>= fun lst ->
      make_feed  
        ~ctxt ~sp 
        (Printf.sprintf 
           (f_ "Private feed for %s")
           accnt.OCAAccount.accnt_real_name)
        (Printf.sprintf
           (f_ "OASIS-DB events watched by %s")
           accnt.OCAAccount.accnt_real_name)
        (preapply feed token)
        lst
  end

let () = 
  Eliom_predefmod.Text.register feed feed_handler

let account_box ~sp ~ctxt srvc limit offset = 

  monitor_token ~ctxt () 
  >>= fun token_opt ->
  monitor_data ~ctxt limit offset
  >|= fun lst ->
  begin
    let rec mk_lst prv_user_date = 
      function 
        | (hd :: tl) as lst ->
            let user_date = 
              (* TODO: Calendar.convert date tz user_tz *)
              hd.ODBLog.log_timestamp
            in
            let same_day =
              match prv_user_date with 
                | Some prv_user_date ->
                    List.fold_left 
                      (fun acc f -> acc && f prv_user_date = f user_date)
                      true
                      [Calendar.day_of_year; Calendar.year]
                | None ->
                    false
            in
              if same_day then
                begin
                  let class_lvl =
                    Log.html_log_level hd
                  in
                    (tr
                       ~a:
                       (match class_lvl with 
                          | Some tk -> [a_class [tk]]
                          | None -> [])

                       (th
                          ~a:[a_class ["hour"]]
                          [pcdata (Printer.Calendar.sprint (s_ "%R") user_date)])

                       [td [pcdata (ODBLog.to_string hd)];
                        td []])
                    ::
                    mk_lst (Some user_date) tl
                end
              else
                begin
                  (tr 
                     (th 
                        ~a:[a_colspan 3; a_class ["day"]]
                        [pcdata (Printer.Calendar.sprint (s_ "%F") user_date)])
                     [])
                  ::
                  mk_lst (Some user_date) lst
                end

        | [] ->
            []
    in

    let feed_box = 
      let realloc =
        post_form 
          ~service:token_realloc
          ~sp
          (fun () ->
             [
               p [string_input ~input_type:`Submit ~value:(s_ "Change this URL") ()];
             ])
          ()
      in
      match token_opt with 
        | Some token ->
            div 
              [pcdata (s_ "Your private feed for monitored events are located here:");
               br ();
               a feed sp 
                 [pcdata 
                    (string_of_uri 
                       (OCAWeb.Redirect.rewrite ~ctxt:ctxt.ocaw sp
                          (make_uri ~absolute:true ~sp ~service:feed token)))]
                 token;
               realloc]
        | None ->
            div 
              [pcdata (s_ "No feed of monitored events for your account.")]
    in

    let extra_headers =
      match token_opt with 
        | Some token ->
            let real_name = 
              match ctxt.accnt with 
                | Some {OCAAccount.accnt_real_name = str} ->
                    str
                | None ->
                    s_ "anonymous"
            in
              Some
                [link 
                   ~a:[a_rel [`Alternate];
                       a_href (make_uri 
                                 ~sp
                                 ~service:feed
                                 token);
                            a_type rss2_type;
                            a_title 
                              (Printf.sprintf 
                                 (f_ "Private feed for %s")
                                 real_name)]
                   ()]
        | None ->
            None
    in

    let navigation_box = 
      let mk_item cond offset txt = 
        if cond then 
          span ~a:[a_class ["enabled"]]
            [a (srvc (max 0 offset)) sp [pcdata txt] ()]
        else
          span ~a:[a_class ["disabled"]] 
            [pcdata txt]
      in

        [
          mk_item 
            (offset > 0)
            (offset - limit)
            (s_ "Previous");

          mk_item
            (List.length lst = limit)
            (offset + limit)
            (s_ "Next");
        ]
    in
      extra_headers,
      div ~a:[a_id "monitor"]
        [
          div ~a:[a_class ["navigate"; "top"]] navigation_box;
          odd_even_table
            (tr 
               (th [pcdata (s_ "Date")])
               [th [pcdata ""];
                th [pcdata (s_ "Description")]])
            (mk_lst None lst);
          div ~a:[a_class ["navigate"; "bottom"]] navigation_box;
          feed_box;
        ]
  end


