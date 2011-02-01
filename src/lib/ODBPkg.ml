
(** Packages 
  
    @author Sylvain Le Gall
  *)

open Lwt

type t1 = string with sexp 

type t = 
    {
      pkg_name:  string;
      pkg_watch: string option;
    } with sexp

type vt = 
  | V1 of t1
  | V2 of t with sexp 

let make nm =
  nm

let upgrade ~ctxt =
  function
    | V1 str -> 
        return 
          {
            pkg_name  = str; 
            pkg_watch = None;
          }
    | V2 t -> 
        return t

(** Load from file *)
let from_file =
  LwtExt.IO.sexp_load vt_of_sexp upgrade

(** Dump to file *)
let to_file =
  LwtExt.IO.sexp_dump sexp_of_vt (fun t -> V1 t)

