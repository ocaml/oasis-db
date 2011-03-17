
(** Account management 
    @author Sylvain Le Gall
  *)

open Context

let user_settings_box ~ctxt sp = 
  OCAWeb.Account.user_settings_box ~ctxt:ctxt.ocaw sp

let new_account_ext ~ctxt sp =
  OCAWeb.Account.new_account ~ctxt:ctxt.ocaw sp

let lost_passwd_ext ~ctxt sp =
  OCAWeb.Account.lost_passwd ~ctxt:ctxt.ocaw sp

