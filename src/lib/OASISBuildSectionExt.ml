
(** Extensions for OASISBuildSection 
  
    @author Sylvain Le Gall
  *)

open OASISTypes 
open OASISExprExt

let status_choose pkg choices =
  match trivial_choose pkg choices with 
    | Some true ->
        `Always
    | Some false ->
        `Never
    | None ->
        `Sometimes

(* Compute buildability of a build section. 
 * TODO: It should take into account default flag value 
 * + reasonable env like in oasis2debian
 *)
let buildable pkg bs = 
  status_choose pkg bs.bs_build

(* Compute installability of a build section. 
 * TODO: It should take into account default flag value 
 * + reasonable env like in oasis2debian
 *)
let installable pkg bs = 
  match buildable pkg bs with
    | `Always ->
        status_choose pkg bs.bs_install
    | e ->
        e
