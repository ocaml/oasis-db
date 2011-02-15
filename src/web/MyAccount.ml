
(** Web services to manage account
    @author Sylvain Le Gall
  *)

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
open Lwt_log

let account_settings = 
  register_new_service
    ~path:["account"; "settings"]
    ~get_params:unit
    (fun sp () () -> 
       Context.get_user ~sp () 
       >>= fun (ctxt, accnt) ->
       Distro.user_settings_box ~sp ~ctxt () 
       >>= fun distro_user_settings_box ->
       Monitor.user_settings_box ~sp ~ctxt () 
       >>= fun monitor_user_settings_box ->
       Account.user_settings_box ctxt.role sp 
       >|= fun account_ext_user_settings_box ->
       template 
         ~ctxt
         ~sp 
         ~title:(OneTitle (s_ "Account settings"))
         ~div_id:"account_settings"
         [
           p [pcdata 
                (s_ "OASIS-DB use two kinds of information: the one stored \
                   in its own database and the one coming from OCaml forge. \
                   You can edit the first one here but you need to go to \
                   OCaml forge to edit the later.")];

           h2 [pcdata (s_ "OCaml forge information")];
           account_ext_user_settings_box;

           h2 [pcdata (s_ "Local information")];
           monitor_user_settings_box;
           distro_user_settings_box;
         ])


let admin =
  register_new_service
    ~path:["admin"]
    ~get_params:unit
    (fun sp () () ->
       Context.get_admin ~sp ()
       >|= fun (ctxt, accnt) ->
       template
         ~ctxt
         ~sp
         ~title:(OneTitle (s_ "Administration"))
         ~div_id:"admin"
         [
           p [pcdata
                (s_ "This page let you access operation that requires 'admin'
                     privileges.");
              em [pcdata (s_ "Use with caution.")]];
         ])

let my_account_handler = 
  register
    Account.my_account
    (fun sp log_offset_opt () ->
       let log_per_page = 20L in
       let log_offset = 
         match log_offset_opt with 
           | Some n -> n 
           | None -> 0L 
       in

       Context.get_user ~sp () 
       >>= fun (ctxt, accnt) ->
       Account.is_admin ~sp () 
       >>= fun is_admin ->
       (* Compute events list *)
       Log.get Log.All Log.LevelAndDate log_offset log_per_page

       (* Display events list *)
       >|= fun events ->
       begin
         let user_tz = 
           Time_Zone.current ()
         in

         let rec mk_lst prv_user_date = 
           function 
             | ((date, tz, log_lvl, descr, lnk) :: tl) as lst ->
                 let user_date = 
                   Calendar.convert date tz user_tz
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
                       let shrt_lvl, class_lvl =
                         Log.html_log_level log_lvl
                       in
                         (tr
                            ~a:[a_class [class_lvl]]
                            (th
                               ~a:[a_class ["hour"]]
                               [pcdata (Printer.Calendar.sprint (s_ "%R") user_date)])
                            [td [pcdata shrt_lvl];
                             td [pcdata descr];
                             td []])
                         ::
                         mk_lst (Some user_date) tl
                     end
                   else
                     begin
                       (tr 
                          (th 
                             ~a:[a_colspan 4; a_class ["day"]]
                             [pcdata (Printer.Calendar.sprint (s_ "%F") user_date)])
                          [])
                       ::
                       mk_lst (Some user_date) lst
                     end

             | [] ->
                 []
         in

         let navigation_box = 
           let mk_item cond offset txt = 
             if cond then 
               span ~a:[a_class ["enabled"]]
                 [a 
                    my_account sp [pcdata txt] 
                    (Some (max 0L offset))]
             else
               span ~a:[a_class ["disabled"]] 
                 [pcdata txt]
           in

             [
               mk_item 
                 (log_offset > 0L)
                 (Int64.sub log_offset log_per_page)
                 (s_ "Previous");

               mk_item
                 (Int64.of_int (List.length events) = log_per_page)
                 (Int64.add log_offset log_per_page)
                 (s_ "Next");
             ]
         in

         let admin_only txt srvc arg = 
           if is_admin then
             a srvc sp [pcdata txt] arg
           else
             span ~a:[a_class ["link_disabled"];
                      a_title (s_ "Need to be admin.")]
               [pcdata txt]
         in

           template 
             ~ctxt
             ~sp 
             ~title:(OneTitle (s_ "My account"))
             ~div_id:"my_account"
             [
              p [pcdata (Printf.sprintf (f_ "Hello %s!") accnt.accnt_name)];

              div ~a:[a_class ["navigate"; "top"]] navigation_box;

              Common.odd_even_table
                (tr 
                   (th [pcdata (s_ "Date")])
                   [th [pcdata ""];
                    th [pcdata (s_ "Description")];
                    th [pcdata (s_ "Link")]])
                (mk_lst None events);

              div ~a:[a_class ["navigate"; "bottom"]] navigation_box;

              ul
                (li [a account_settings sp 
                       [pcdata (s_ "Settings")] ()])
                (* TODO *)
                [li [admin_only (s_ "Delete packages and versions") admin ()];
                 li [a admin sp [pcdata (s_ "Create package")] ()];
                 li [admin_only (s_ "Manage tasks") admin ()]];
             ]
       end)
