
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open ODBGettext
open Template

let new_account_handler = 
  Defer.register
    Account.new_account
    (fun sp () () ->
       unauth_template 
         ~sp 
         ~title:(OneTitle (s_ "New account"))
         ~div_id:"new_account"
         ()
       >>= fun (_, tmpl) ->
       tmpl
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

           p [a (Account.new_account_ext sp) sp 
                [pcdata (s_ "Create an account on OCaml forge")] ()];


           h3 [pcdata (s_ "Lost password")];
           p [pcdata 
                (s_ 
                   "Passwords are managed by OCaml forge. In order to recover \
                    your password, just follow the password recovery process \
                    of the forge.")];

           p [a (Account.lost_passwd_ext sp) sp
                [pcdata (s_ "Reset your password on OCaml forge")] ()];
         ])

let init () = 
  new_account_handler ()
