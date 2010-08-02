
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

type log_level =
  | Error
  | Warning
  | Info

let string_of_log_level =
  function
    | Error   -> s_ "Error"
    | Warning -> s_ "Warning"
    | Info    -> s_ "Info"

let _ = 
  register
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
       >>=
       (fun events ->
          page_template sp (s_ "My account") Account.box
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

             p [a Account.account_settings sp 
                  [pcdata (s_ "Account settings")] ()];
            ]))
