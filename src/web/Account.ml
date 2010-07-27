
(** Account management and page ACL
    @author Sylvain Le Gall
  *)

open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open BocageGettext
open Template
open SQL

type role = 
  | Anon
  | User 
  | Admin

let string_of_role =
  function
    | Anon  -> "anonymous"
    | User  -> "user"
    | Admin -> "administrator"

type account =
    {
      id:   int32;
      name: string;
      role: role;
    }

let account sp = 
  let the =
    function 
      | Some i -> i
      | None -> assert(false)
  in
   catch 
     (fun () ->
        (let cookies =
           Eliom_sessions.get_cookies sp
         in
           try 
             return 
               (Netencoding.Url.decode
                  (Ocsigen_lib.String_Table.find 
                     "account_ext_token"
                     cookies))
           with Not_found ->
             fail 
               (Failure (s_ "Forge cookie not found")))

        >>=
        (fun token ->
           Lwt_pool.use SQL.pool
             (fun db -> 
                PGSQL(db) 
                  "SELECT user_id, realname FROM account_ext \
                     WHERE token = $token"))

        >>= 
        (function
           | e :: _ -> 
               return e
           | [] -> 
               fail (Failure (s_ "Cannot found token in DB")))

        >>= 
        (fun (user_id, realname) ->
           return
             (Some
                {
                  id   = the user_id;
                  name = the realname;
                  role = User;
                }))
     )
     
     (function
        | Failure msg ->
            return None
        | e ->
            fail e)

let self_uri ~sp = 
  let proto =
    if get_ssl ~sp then
      "https"
    else
      "http"
  in
  let port =
    match get_server_port ~sp with 
      | 80 -> ""
      | n -> ":"^(string_of_int n)
  in
  let suff =
    match get_suffix ~sp with
      | Some lst -> "/"^(String.concat "/" lst)
      | None -> ""
  in
    Printf.sprintf 
      "%s://%s%s%s%s"
      proto
      (get_hostname ~sp)
      port
      (get_full_url ~sp)
      suff

(* TODO: refactor with above *)
let service_uri ~sp service =
  let proto =
    if get_ssl ~sp then
      "https"
    else
      "http"
  in
  let port =
    match get_server_port ~sp with 
      | 80 -> ""
      | n -> ":"^(string_of_int n)
  in
  let suff =
    make_string_uri 
      ~absolute_path:true 
      ~service
      ~sp 
      ()
  in
    (* TODO: use make_proto_prefix *)
    Printf.sprintf 
      "%s://%s%s%s"
      proto
      (get_hostname ~sp)
      port
      suff

let account_ext = 
  Eliom_services.new_external_service
    ~prefix:"http://localhost"
    ~path:["~gildor"; "account-ext.php"]
    ~get_params:(string "action" ** 
                 string "redirect") 
    ~post_params:unit
    ()

let mk_account sp action =
  preapply account_ext (action, self_uri ~sp)

let logout_ext sp =
  mk_account sp "logout"

let my_account =
  new_service ["my_account"] unit ()

let login_ext sp =
  preapply 
    account_ext 
    ("login", 
     service_uri ~sp my_account)

let new_account_ext sp =
  mk_account sp "new"

let manage_account_ext sp =
  mk_account sp "manage"

let lost_passwd_ext sp =
  mk_account sp "lost_passwd"

let new_account = 
  new_service ["new_account"] unit ()

let box sp = 
  (account sp)
  >>=
  (function 
    | Some _ ->
        return 
          [ul 
             (li [a (logout_ext sp) sp [pcdata (s_ "Log Out")] ()])
             [li [a my_account sp  [pcdata (s_ "My account")] ()]]]
     | None ->
         return 
           [ul
              (li [a (login_ext sp) sp  [pcdata (s_ "Log In")] ()])
              [li [a new_account sp [pcdata (s_ "New account")] ()]]])

let _ = 
  register
    new_account
    (fun sp () () ->
       page_template sp (s_ "New account") box
         [
           h2 [pcdata (s_ "New account")];
           p [pcdata 
                (s_ 
                   "Browsing OASIS-DB web interface is available to all. \
                   However, editing OASIS-DB and uploading packages require \
                   a username and password.")];

           h3 [pcdata (s_ "Getting an OASIS-DB username")];
           p [pcdata 
                (s_
                   "OASIS-DB and OCaml forge share accounts information. 
                    If you need a username, you must create an account on \
                    OCaml forge.")];

           p [a (new_account_ext sp) sp 
                [pcdata (s_ "Create an account on OCaml forge")] ()];


           h3 [pcdata (s_ "Lost password")];
           p [pcdata 
                (s_ 
                   "Passwords are managed by OCaml forge. In order to recover \
                    your password, just follow the password recovery process \
                    of the forge.")];

           p [a (lost_passwd_ext sp) sp
                [pcdata (s_ "Reset your password on OCaml forge")] ()];
         ])

let account_settings = 
  register_new_service
    ~path:["account"; "settings"]
    ~get_params:unit
    (fun sp () () -> 
       (account sp)
       >>=
       (fun accnt ->
          page_template sp (s_ "Account settings") box 
            (match accnt with 
               | Some accnt ->
                   [
                     h2 [pcdata (s_ "Manage account")];
                     p [pcdata 
                          (s_ "OASIS-DB use two kinds of information: the one stored \
                             in its own database and the one coming from OCaml forge. \
                             You can edit the first one here but you need to go to \
                             OCaml forge to edit the later.")];

                     h2 [pcdata (s_ "OCaml forge information")];

                     table
                       (tr
                          (td [pcdata (s_ "Name")])
                          [td [pcdata accnt.name]])
                       [tr
                          (td [pcdata (s_ "Role")])
                          [td [pcdata (string_of_role accnt.role)]]
                       ];

                     p [a (manage_account_ext sp) sp
                          [pcdata (s_ "Edit settings on OCaml forge")] ()];

                     h2 [pcdata (s_ "Local information")];
                   ]
              | None ->
                  [p [pcdata "Not logged in"]])))

open CalendarLib

type log_level =
  | Error
  | Warning
  | Info

let string_of_log_level =
  function
    | Error   -> s_ "Error"
    | Warning -> s_ "Warning"
    | Info    -> s_ "Info"

let browse_pkg =
  register_new_service 
    ~path:["browse_pkg"] 
    ~get_params:(suffix (string "pkg"))
    (fun sp pkg () ->
       let ttl = 
         Printf.sprintf (f_ "Package %s") pkg
       in
         page_template sp ttl box
           [h2 [pcdata ttl]])

let browse_pkg_ver =
  register_new_service 
    ~path:["browse_pkg_ver"] 
    ~get_params:(suffix (string "pkg" ** string "ver")) 
    (fun sp (pkg, ver) () ->
       let ttl = 
         Printf.sprintf (f_ "Package %s v%s") pkg ver
       in
         page_template sp ttl box
           [h2 [pcdata ttl]])

let edit_info =
  register_new_service
    ~path:["edit_info"]
    ~get_params:(suffix (string "pkg" ** string "ver"))
    (fun sp (pkg, ver) () ->
       let ttl =
         Printf.sprintf (f_ "Edit %s v%s") pkg ver
       in
         page_template sp ttl box
            [h2 [pcdata ttl]])

let a_edit_info sp (pkg,ver) = 
  a (preapply edit_info (pkg, ver)) sp 
    [pcdata (s_ "Edit version")] ()

let a_browse_pkg_ver sp (pkg, ver) =
  a (preapply browse_pkg_ver (pkg, ver)) sp
    [pcdata (s_ "Browse version")] ()

let _ = 
  register
    my_account
    (fun sp () () ->

       (* Compute events list *)
       (return
          [Calendar.make 2010 07 08 17 00 01,
           Error,
           "Cannot parse '_oasis' file of ocamlnet-3.0.tar.gz",
           a_edit_info sp ("ocamlnet", "3.0");
           
           Calendar.make 2010 07 08 18 15 01,
           Error,
           "Cannot parse '_oasis' file of sexplib310-3.4.tar.bz2",
           a_edit_info sp ("sexplib310", "3.4");

           Calendar.make 2010 07 08 18 15 01,
           Warning,
           "ocaml-gettext version 0.3.1 uses outdated OASISFormat v0.1",
           a_edit_info sp ("ocaml-gettext", "0.3.1");

           Calendar.make 2010 07 05 18 15 01,
           Info,
           "New upstream ocaml-gettext version 0.3.1",
           a_browse_pkg_ver sp ("ocaml-gettext", "0.3.1");

           Calendar.make 2010 07 07 18 15 01,
           Info,
           "New upstream ocaml-fileutils version 0.4.1",
           a_browse_pkg_ver sp ("ocaml-fileutils", "0.4.1")])

       (* Display events list *)
       >>=
       (fun events ->
          page_template sp (s_ "My account") box
            [h2 [pcdata (s_ "My account")];

             table
               (tr 
                  (th [pcdata (s_ "Date")])
                  [th [pcdata (s_ "Type")];
                   th [pcdata (s_ "Description")];
                   th [pcdata (s_ "Link")]])
               (List.map 
                  (fun (date, log_lvl, descr, lnk) ->
                     tr
                       (td [pcdata (Printer.Calendar.to_string date)])
                       [td [pcdata (string_of_log_level log_lvl)];
                        td [pcdata descr];
                        td [lnk]])
                  events);

             p [a account_settings sp 
                  [pcdata (s_ "Account settings")] ()];
            ]))
