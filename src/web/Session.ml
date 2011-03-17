
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

