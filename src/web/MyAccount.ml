
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
open OCAAccount
open Lwt_log

let () = 
  Sqlexpr.register
    "account"
    1
    (fun db ->
       Sqlexpr.execute db
         sqlinit"CREATE TABLE IF NOT EXISTS account\
         (id INTEGER PRIMARY KEY NOT NULL)")
    (fun _ _ -> return ())

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
       Account.user_settings_box ~ctxt sp 
       >>= fun account_ext_user_settings_box ->
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
    ~path:["admin"; "oasis-db"]
    ~get_params:unit
    (fun sp () () ->
       Context.get_admin ~sp ()
       >>= fun (ctxt, accnt) ->
       Cron.box ~ctxt ~sp ()
       >>= fun cron_box ->
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
           cron_box;
         ])
             
let my_account_with_offset =
  new_service ["my_account"]
    (opt (string "redirect") ** int "offset")
    ()

let my_account_handler sp redirect_opt log_offset = 
  (* TODO: use redirect_opt *)
  let log_per_page = 
    20 
  in

  Context.get_user ~sp () 
  >>= fun (ctxt, accnt) ->
  Monitor.account_box ~sp ~ctxt 
    (fun offset -> 
       (preapply my_account_with_offset (redirect_opt, offset))) 
    log_per_page log_offset 
  >>= fun (extra_headers, monitor_box) ->
  begin
      template 
        ~ctxt
        ~sp 
        ?extra_headers
        ~title:(OneTitle (s_ "My account"))
        ~div_id:"my_account"
        [
         p [pcdata (Printf.sprintf (f_ "Hello %s!") accnt.accnt_real_name)];

         monitor_box;

         ul
           (li [a account_settings sp 
                  [pcdata (s_ "Settings")] ()])
           (* TODO *)
           [li [a admin sp [pcdata (s_ "Administration")] ()]];
        ]
  end

let () = 
  register
    Common.my_account
    (fun sp redirect_opt () ->
       my_account_handler sp redirect_opt 0);
  register
    my_account_with_offset
    (fun sp (redirect_opt, offset) () ->
       my_account_handler sp redirect_opt offset)



