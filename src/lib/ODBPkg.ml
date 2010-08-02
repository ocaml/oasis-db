
(** Packages 
  
    @author Sylvain Le Gall
  *)

open Lwt

type t = string with sexp

type vt = V1 of t with sexp 

let make nm =
  nm

let upgrade ~ctxt =
  function
    | V1 t -> return t

(** Load from file *)
let from_file =
  LwtExt.IO.sexp_load vt_of_sexp upgrade

(** Dump to file *)
let to_file =
  LwtExt.IO.sexp_dump sexp_of_vt (fun t -> V1 t)

