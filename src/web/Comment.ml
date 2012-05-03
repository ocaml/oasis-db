
(** Web services to display/register comments per package
    @author Sylvain Le Gall
  *)

open Lwt
open XHTML.M
open ODBGettext
open Common
open Eliom_parameters
open Eliom_predefmod.Xhtml

module S = Sqlexpr

let () = 
  S.register 
    "comment"
    1
    (fun db ->
       S.execute db
         sqlinit"CREATE TABLE IF NOT EXISTS comment\
         (id INTEGER PRIMARY KEY AUTOINCREMENT, \
          user_id INTEGER,
          pkg TEXT NOT NULL,
          ver TEXT,
          markdown TEXT,
          timestamp DATETIME DEFAULT (datetime('now')),
          FOREIGN KEY(user_id) REFERENCES user(id))")
    (fun _ _ ->
       return ())

let edited_state = 
  Eliom_sessions.create_volatile_table ()

let edited_state_get ~sp () = 
  match Eliom_sessions.get_volatile_session_data ~sp ~table:edited_state () with 
    | Eliom_sessions.Data lst ->
        lst
    | Eliom_sessions.Data_session_expired
    | Eliom_sessions.No_data ->
        []

let edited_state_remove ~sp id = 
  let data = 
    Eliom_sessions.get_volatile_session_data 
      ~table:edited_state 
      ~sp 
      () 
  in
    match data with 
      | Eliom_sessions.Data lst ->
          let lst = 
            List.filter (fun id' -> id <> id') lst
          in
            Eliom_sessions.set_volatile_session_data 
              ~table:edited_state 
              ~sp 
              lst
 
      | _ ->
          ()

let save = 
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"save_comment"
    ~post_params:(string "pkg" ** 
                  opt (ExtParams.version "ver") ** 
                  string "mkd" **
                  opt (int "id"))
    (fun sp () (pkg, (ver_opt, (mkd, id_opt))) ->
       Context.get_user ~sp ()
       >>= fun (ctxt, accnt) ->
       begin
         let db = ctxt.Context.sqle in
         let user_id = accnt.OCAAccount.accnt_id in
         let ver_str_opt = 
           match ver_opt with
             | Some v -> Some (OASISVersion.string_of_version v)
             | None -> None
         in
           match id_opt with 
             | Some id ->
                 if Context.is_admin ~ctxt () then
                   S.execute db 
                     sql"UPDATE comment SET markdown = %s WHERE id = %d"
                     mkd id
                 else
                   S.execute db
                     sql"UPDATE comment SET markdown = %s WHERE id = %d AND user_id = %d"
                     mkd id user_id 
             | None ->
                 S.execute db
                   sql"INSERT INTO comment(user_id, pkg, ver, markdown) \
                       VALUES (%d, %s, %s?, %s)"
                   user_id pkg ver_str_opt mkd
       end
       >>= fun () ->
       Log.add ctxt.Context.sqle (`Pkg (pkg, `Commented))
       >>= fun () ->
       begin
         (* Remove the id from the edited state *)
         match id_opt with 
           | Some id ->
               return (edited_state_remove ~sp id)
           | None ->
               return ()
       end)

let delete =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"delete_comment"
    ~post_params:(int "id")
    (fun sp () id ->
       Context.get_user ~sp ()
       >>= fun (ctxt, accnt) ->
       (* Allow to delete by itself or by admin *)
       begin
         let db = ctxt.Context.sqle in
         if Context.is_admin ~ctxt () then
           S.execute db
             sql"DELETE FROM comment WHERE id = %d"
             id
         else
           S.execute db
             sql"DELETE FROM comment WHERE id = %d AND user_id = %d"
             id accnt.OCAAccount.accnt_id
       end
       >>= fun () ->
       return (edited_state_remove ~sp id))


let edit = 
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"edit_comment"
    ~post_params:(int "id")
    (fun sp () id ->
       Context.get_user ~sp () 
       >>= fun (ctxt, accnt) ->
       (* Allow to edit by itself or by admin *)
        begin
          let lst = 
            edited_state_get ~sp ()
          in
            Eliom_sessions.set_volatile_session_data ~sp 
              ~table:edited_state (id :: lst);
            return ()
        end)

let pkg_box_handler ~sp ~ctxt pkg ver_opt = 
  let edited = 
    edited_state_get ~sp ()
  in

  let mk_edit_area pre_opt id_opt mkd =
    div 
      ~a:[a_class ["postEdit"]]
      [
        post_form ~service:save ~sp
          (fun (pkg_nm, (ver_opt_nm, (mkd_nm, id_opt_nm))) ->
             let res = 
               [
                 string_input
                   ~input_type:`Hidden
                   ~name:pkg_nm
                   ~value:pkg
                   ();
                 textarea
                   ~name:mkd_nm
                   ~cols:40
                   ~rows:10
                   ~value:mkd
                   ();
                 string_input 
                   ~input_type:`Submit 
                   ~value:(s_ "Save") 
                   ();
               ]
             in
             let res = 
               match id_opt with 
                 | Some id ->
                     (int_input
                        ~input_type:`Hidden
                        ~name:id_opt_nm
                        ~value:id
                        ())
                     :: res
                 | None ->
                     res
             in
             let res = 
               match pre_opt with 
                 | Some pre ->
                     (pre :: res)
                 | None ->
                     res
             in
             let res = 
               match ver_opt with 
                 | Some ver -> 
                     user_type_input
                       ~input_type:`Hidden
                       ~name:ver_opt_nm
                       ~value:ver
                       OASISVersion.string_of_version
                       ()
                     :: res
                 | None ->
                     res
             in
               [p res]
          )
          ()
      ]
  in

  let decode (id, user_id, ver_str_opt, mkd, timestamp) = 
    Account.of_id ~ctxt user_id
    >>= fun user_accnt ->

    begin
      let user_date = 
        (* TODO: Calendar.convert data tz user_tz *)
        CalendarLib.Printer.Calendar.from_string timestamp
      in
      
      let is_self =
        match ctxt.Context.accnt with
          | Some {OCAAccount.accnt_id = user_id'} ->
              user_id' = user_id
          | _ ->
              false
      in

      let is_edited = 
        List.mem id edited
      in

      let res = 
        div
          ~a:[a_id (Printf.sprintf "id%d" id);
              a_class ["post"]]
          [
            div 
              ~a:[a_class ["postInfo"]]
              [
                span
                  ~a:[a_class ["postUser"]]
                  [pcdata user_accnt.OCAAccount.accnt_real_name];
                span
                  ~a:[a_class ["postTimestamp"]]
                  [pcdata 
                     (CalendarLib.Printer.Calendar.to_string 
                        user_date)];
                span
                  ~a:[a_class ["postVersion"]]
                  [pcdata 
                     (match ver_str_opt with 
                        | Some ver_str ->
                            Printf.sprintf (f_ "v%s") ver_str
                        | None ->
                            s_ "no version")];
              ];

            div
              ~a:[a_class ["postContent"]]
              (if is_edited then 
                 [mk_edit_area None (Some id) mkd]
               else
                 MarkdownExt.to_html mkd);

            div
              ~a:[a_class ["postActions"]]
              (if not is_edited && (is_self || Context.is_admin ~ctxt ()) then
                 [
                   ul 
                     (li 
                        [post_form ~service:edit ~sp 
                           (fun id_nm ->
                              [p
                                 [int_button 
                                    ~name:id_nm
                                    ~value:id
                                    [pcdata (s_ "Edit")]]])
                           ()])
                     [li
                        [post_form ~service:delete ~sp
                           (fun id_nm ->
                              [p 
                                 [int_button
                                    ~name:id_nm
                                    ~value:id
                                    [pcdata (s_ "Delete")]]])
                           ()]]
                 ]
               else
                 []);
          ]
      in

        return (is_edited, res)
    end
  in
  let db = ctxt.Context.sqle in
    S.fold db 
      (fun (is_edited, lst) e ->
         decode e 
         >>= fun (is_edited', div) -> 
         return (is_edited || is_edited', div :: lst))
      (false, [])
      sql"SELECT @d{id}, @d{user_id}, @s?{ver}, @s{markdown}, @s{timestamp} FROM comment \
          WHERE pkg = %s ORDER BY timestamp ASC"
      pkg
    >>= fun (is_edited, lst) ->
    begin
      let lst =
        if not is_edited && Context.is_user ~ctxt () then
          (div 
             ~a:[a_class ["postNew"]]
             [
               mk_edit_area 
                 (Some (b [pcdata (s_ "Add a comment")])) 
                 None 
                 ""
             ])
          :: lst
        else
          lst
      in
        return 
          (div ~a:[a_class ["comment"]]
             ((h3 [pcdata (sn_ "Comment:" "Comments:" (List.length lst))])
             ::
              (match lst with 
                 | [] ->
                     [p ~a:[a_class ["no_comments"]] [pcdata (s_ "No comments")]]
                 | _ ->
                     (List.rev lst))))
    end

let pkg_ver_box ~sp ~ctxt pkg_ver =
  pkg_box_handler ~sp ~ctxt 
    pkg_ver.ODBPkgVer.pkg
    (Some pkg_ver.ODBPkgVer.ver)


let pkg_box ~sp ~ctxt pkg =
  pkg_box_handler ~sp ~ctxt pkg None
