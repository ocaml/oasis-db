
(** Web services to manage monitored packages
    @author Sylvain Le Gall
  *)

open Lwt
open XHTML.M
open ODBGettext
open Common
open Eliom_services

(* TODO *)

let user_settings_box ~sp ~ctxt () = 
  return 
    (div [p [pcdata (s_ "Monitor")]])

let monitor = 
  Eliom_predefmod.Action.register_new_coservice'
    ~name:"monitor_pkg"
    ~get_params:(ExtParams.pkg "pkg")
    (fun sp k () ->
       Context.get_user ~sp () 
       >>= fun (ctxt, _) ->
       return ())

let box ~sp ~ctxt k = 
  return 
    (Session.link_need_login ~sp ~ctxt
       (s_ "Stop monitor")
       (preapply monitor k))
