
(** Web services to manage monitored packages
    @author Sylvain Le Gall
  *)

open Lwt
open XHTML.M
open ODBGettext

(* TODO *)

let user_settings_box ~sp ~ctxt () = 
  return 
    (div [p [pcdata (s_ "Monitor")]])

let pkg_ver_box ~sp ~ctxt () = 
  return 
    (span [pcdata (s_ "Stop monitor")])
