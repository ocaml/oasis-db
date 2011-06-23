
(** Account management 
    @author Sylvain Le Gall
  *)

open Context
open OCAAccount

let user_settings_box ~ctxt sp = 
  OCAWeb.Account.user_settings_box ~ctxt:ctxt.ocaw sp

let new_account_ext ~ctxt sp =
  OCAWeb.Account.new_account ~ctxt:ctxt.ocaw sp

let lost_passwd_ext ~ctxt sp =
  OCAWeb.Account.lost_passwd ~ctxt:ctxt.ocaw sp

let get_id ~ctxt () = 
  match ctxt.accnt with
    | Some {accnt_id = res} -> 
        Some res
    | None -> 
        None
let of_id ~ctxt user_id = 
  of_id ~ctxt:(OCAWeb.to_oca_context ctxt.ocaw) user_id

let list ~ctxt () = 
  list ~ctxt:(OCAWeb.to_oca_context ctxt.ocaw) ()

