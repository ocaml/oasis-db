
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
       >|= fun (ctxt, accnt) ->
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
       (* Compute events list *)
       Log.get Log.All Log.LevelAndDate log_offset log_per_page

       (* Display events list *)
       >|= fun events ->
       begin
         let user_tz = 
           Time_Zone.current ()
         in

         let rec mk_lst odd prv_user_date = 
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
                            ~a:[a_class [if odd then "odd" else "even"; class_lvl]]
                            (th
                               ~a:[a_class ["hour"]]
                               [pcdata (Printer.Calendar.sprint (s_ "%R") user_date)])
                            [td [pcdata shrt_lvl];
                             td [pcdata descr];
                             td []])
                         ::
                         mk_lst (not odd) (Some user_date) tl
                     end
                   else
                     begin
                       (tr 
                          (th 
                             ~a:[a_colspan 4; a_class ["day"]]
                             [pcdata (Printer.Calendar.sprint (s_ "%F") user_date)])
                          [])
                       ::
                       mk_lst true (Some user_date) lst
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

           template 
             ~ctxt
             ~sp 
             ~title:(OneTitle (s_ "My account"))
             ~div_id:"my_account"
             [
              p [pcdata (Printf.sprintf (f_ "Hello %s!") accnt.accnt_name)];

              div ~a:[a_class ["navigate"; "top"]] navigation_box;

              table
                (tr 
                   (th [pcdata (s_ "Date")])
                   [th [pcdata ""];
                    th [pcdata (s_ "Description")];
                    th [pcdata (s_ "Link")]])
                (mk_lst true None events);

              div ~a:[a_class ["navigate"; "bottom"]] navigation_box;

              p [a account_settings sp 
                   [pcdata (s_ "Account settings")] ()];
             ]
       end)
