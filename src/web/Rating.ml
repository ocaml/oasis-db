
(** Web services to display/register ratings per package
    @author Sylvain Le Gall
  *)

open Lwt
open XHTML.M
open ODBGettext
open Eliom_parameters
open Eliom_predefmod.Xhtml
open Common

module S = Sqlexpr

let () = 
  S.register 
    "rating"
    1
    (fun db ->
       S.execute db
         sqlinit"CREATE TABLE IF NOT EXISTS rating\
          (id INTEGER PRIMARY KEY AUTOINCREMENT, \
           user_id INTEGER,
           pkg TEXT NOT NULL, \
           ver TEXT NOT NULL, \
           mark INTEGER NOT NULL,
           FOREIGN KEY(user_id) REFERENCES user(id))")
    (fun _ _ -> 
       return ())

let mk_rate_box mark_opt lst =  
  let mark =
    match mark_opt with 
      | Some m -> m
      | None -> 0.
  in
    ul
      ~a:[a_class ["star-rating"]]
      (li 
         ~a:[a_class ["current-rating"];
             a_style (Printf.sprintf "width:%dpx" 
                        (int_of_float (80.0 *. mark /. 5.0)))]
         [pcdata (Printf.sprintf (f_ "%0.1f") mark)])
      (List.map 
         (fun (clss, ctnt) ->
            li ~a:[a_class [clss]] [ctnt])
         lst)

let pkg_box ~sp ~ctxt pkg = 
  let db = ctxt.Context.sqle in
  let mark_opt = 
    S.select_one db
      sql"SELECT @f?{avg(mark)} FROM rating WHERE pkg = %s"
      pkg
  in
  let count_opt = 
    (* TODO: merge this with mark_opt *)
    S.select_one db
      sql"SELECT @d?{count(mark)} FROM rating WHERE pkg = %s"
      pkg
  in
    mark_opt  >>= fun mark_opt ->
    count_opt >>= fun count_opt -> 
    begin
      let count = 
        match count_opt with Some c -> c | None -> 0
      in
      let res = 
        mk_rate_box mark_opt 
          ["vote-rating",
           pcdata
             (Printf.sprintf
                (fn_ "(%d vote)" "(%d votes)" count)
                count)]
      in
        return res
    end

let rate_action =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"pkg_rate"
    ~post_params:(string "pkg" ** ExtParams.version "ver" ** int "mark")
    (fun sp () (pkg, (ver, mark)) ->
       Context.get_user ~sp () 
       >>= fun (ctxt, accnt) ->
       let db = ctxt.Context.sqle in
         S.execute db
           sql"DELETE FROM rating WHERE pkg = %s AND ver = %s AND user_id = %d"
           pkg 
           (OASISVersion.string_of_version ver)
           accnt.OCAAccount.accnt_id
         >>= fun _ ->
         S.execute db
           sql"INSERT INTO rating (pkg, ver, user_id, mark) VALUES (%s, %s, %d, %d)"
           pkg 
           (OASISVersion.string_of_version ver)
           accnt.OCAAccount.accnt_id
           mark
       >>= fun _ ->
       Log.add ctxt.Context.sqle (`Pkg (pkg, `Rated)))

let pkg_ver_box ~sp ~ctxt pkg_ver = 
  let f mark_opt = 
    post_form ~service:rate_action ~sp ~a:[a_class ["star-rating"]]
      (fun (pkg_nm, (ver_nm, mark_nm)) ->
         let link_rate n = 
           int_button
             ~name:mark_nm
             ~value:n
             [pcdata (Printf.sprintf (f_ "Rate %d/5") n)]
         in
           [
             p
               ~a:[a_class ["hidden"]]
               [
                 string_input
                  ~name:pkg_nm
                  ~input_type:`Hidden
                  ~value:pkg_ver.ODBPkgVer.pkg
                  ();

                 user_type_input 
                   ~name:ver_nm
                   ~input_type:`Hidden
                   ~value:pkg_ver.ODBPkgVer.ver
                   OASISVersion.string_of_version
                   ();
               ];

             mk_rate_box 
               (match mark_opt with
                  | Some i -> Some (float_of_int i)
                  | None -> None)
               (List.map 
                  (fun i -> 
                     "star"^(string_of_int i),
                     link_rate i)
                  [1; 2; 3; 4; 5])
           ])
      ()
  in

    pkg_box ~sp ~ctxt pkg_ver.ODBPkgVer.pkg
    >>= fun general_mark ->
    begin
      let db = ctxt.Context.sqle in
        match Account.get_id ~ctxt () with 
          | Some id ->
              catch
                (fun () ->
                   S.select_one db
                     sql"SELECT @d{mark} FROM rating WHERE pkg = %s AND ver = %s AND user_id = %d"
                     pkg_ver.ODBPkgVer.pkg
                     (OASISVersion.string_of_version pkg_ver.ODBPkgVer.ver)
                     id
                   >|= fun m ->
                     f (Some m))

                (function
                   | Not_found ->
                       return (f None)
                           
                   | e ->
                       fail e)

          | None ->
              return 
                (span [pcdata (s_ "Rate this package")])
    end
    >>= fun my_mark ->

    return (general_mark, my_mark)

