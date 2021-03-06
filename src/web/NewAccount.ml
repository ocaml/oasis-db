
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open ODBGettext
open Template

let new_account_handler = 
  register
    Common.new_account
    (fun sp redirect_opt () ->
       (* TODO: use redirect_opt *)
       Context.get ~sp () 
       >>= fun ctxt ->
       template 
         ~ctxt
         ~sp 
         ~title:(OneTitle (s_ "New account"))
         ~div_id:"new_account"
         [
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

           p [XHTML.M.a
                ~a:[a_href (Account.new_account_ext ~ctxt sp)]
                [pcdata (s_ "Create an account on OCaml forge")]];

           h3 [pcdata (s_ "Lost password")];
           p [pcdata 
                (s_ 
                   "Passwords are managed by OCaml forge. In order to recover \
                    your password, just follow the password recovery process \
                    of the forge.")];

           p [XHTML.M.a 
                ~a:[a_href (Account.lost_passwd_ext ~ctxt sp)]
                [pcdata (s_ "Reset your password on OCaml forge")]];
         ])
