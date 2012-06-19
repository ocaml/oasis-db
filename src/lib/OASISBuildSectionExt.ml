
(** Extensions for OASISBuildSection 
  
    @author Sylvain Le Gall
  *)

open OASISTypes 
open OASISExprExt

let status_choose what preset_data pkg cs choices =
  let res, str = 
    match trivial_choose preset_data pkg choices with 
      | Some true ->
          `Always, "always"
      | Some false ->
          `Never, "never"
      | None ->
          `Sometimes, "sometimes"
  in
    res

(* Compute buildability of a build section. *)
let buildable preset_data pkg cs bs = 
  status_choose "buildable" preset_data pkg cs bs.bs_build

(* Compute installability of a build section. *)
let installable preset_data pkg cs bs = 
  match buildable preset_data pkg cs bs with
    | `Always ->
        status_choose "installable" preset_data pkg cs bs.bs_install
    | e ->
        e
