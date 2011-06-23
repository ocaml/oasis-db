
(** Session management
    @author Sylvain Le Gall
  *)

open Context

let action_box ~ctxt sp = 
  OCAWeb.Session.action_box ~ctxt:ctxt.ocaw sp

let login_get_ext ~ctxt sp = 
  OCAWeb.Session.login_get ~ctxt:ctxt.ocaw sp 

let logout_get_ext ~ctxt sp =  
  OCAWeb.Session.logout_get ~ctxt:ctxt.ocaw sp 

open XHTML.M
open Eliom_predefmod.Xhtml

let link_need_login ~sp ~ctxt txt srvc = 
  match Account.get_id ~ctxt () with
    | Some _ ->
        a srvc sp [pcdata txt] ()
    | None ->
        OCAWeb.Session.with_login
          ~ctxt:ctxt.ocaw
          sp 
          (make_uri ~absolute:true ~sp ~service:srvc ())
          [pcdata txt]
