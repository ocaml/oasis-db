
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open CalendarLib
open ODBGettext
open Lwt
open Template
open Context
open Account

type log_level =
  | Error
  | Warning
  | Info

let string_of_log_level =
  function
    | Error   -> s_ "Error"
    | Warning -> s_ "Warning"
    | Info    -> s_ "Info"

let account_settings = 
  Defer.register_new_service
    ~path:["account"; "settings"]
    ~get_params:unit
    (fun sp () () -> 
       auth_template 
         ~sp 
         ~title:(OneTitle (s_ "Account settings"))
         ~div_id:"account_settings"
         () 
       >>= fun (ctxt, tmpl, accnt) ->
       tmpl
         [
           p [pcdata 
                (s_ "OASIS-DB use two kinds of information: the one stored \
                   in its own database and the one coming from OCaml forge. \
                   You can edit the first one here but you need to go to \
                   OCaml forge to edit the later.")];

           h2 [pcdata (s_ "OCaml forge information")];

           table
             (tr
                (td [pcdata (s_ "Name")])
                [td [pcdata accnt.accnt_name]])
             [tr
                (td [pcdata (s_ "Role")])
                [td [pcdata (string_of_role ctxt.role)]]
             ];

           p [a (manage_account_ext sp) sp
                [pcdata (s_ "Edit settings on OCaml forge")] ()];

           h2 [pcdata (s_ "Local information")];
         ])

let my_account_handler = 
  Defer.register
    Account.my_account
    (fun sp () () ->

       (* Compute events list *)
       (return
          [Calendar.make 2010 07 08 17 00 01,
           Error,
           "Cannot parse '_oasis' file of ocamlnet-3.0.tar.gz",
           Browse.a_edit_info sp ("ocamlnet", "3.0");
           
           Calendar.make 2010 07 08 18 15 01,
           Error,
           "Cannot parse '_oasis' file of sexplib310-3.4.tar.bz2",
           Browse.a_edit_info sp ("sexplib310", "3.4");

           Calendar.make 2010 07 08 18 15 01,
           Warning,
           "ocaml-gettext version 0.3.1 uses outdated OASISFormat v0.1",
           Browse.a_edit_info sp ("ocaml-gettext", "0.3.1");

           Calendar.make 2010 07 05 18 15 01,
           Info,
           "New upstream ocaml-gettext version 0.3.1",
           Browse.a_browse_pkg_ver sp ("ocaml-gettext", "0.3.1");

           Calendar.make 2010 07 07 18 15 01,
           Info,
           "New upstream ocaml-fileutils version 0.4.1",
           Browse.a_browse_pkg_ver sp ("ocaml-fileutils", "0.4.1")])

       (* Display events list *)
       >>= fun events ->
       auth_template 
         ~sp 
         ~title:(OneTitle (s_ "My account"))
         ~div_id:"my_account"
         ()
       >>= fun (_, tmpl, accnt) ->
       tmpl
         [
          p [pcdata (Printf.sprintf (f_ "Hello %s!") accnt.accnt_name)];
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

          p [a (account_settings ()) sp 
               [pcdata (s_ "Account settings")] ()];
         ])

let init () =
  my_account_handler ();
  ignore (account_settings ())
