
(** Web services to manage distribution display
    @author Sylvain Le Gall
  *)


open Lwt
open XHTML.M
open ODBGettext

(* TODO *)

let user_settings_box ~sp ~ctxt () = 
  return
    (div [p [pcdata (s_ "Preferred distribution")]])

let pkg_ver_box ~sp ~ctxt pkg_ver = 
  return
    (span [pcdata (s_ "apt-get install TODO")])
