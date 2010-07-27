
(** Package's version 
 
    @author Sylvain Le Gall
  *)

open Lwt
open ODBTypes

TYPE_CONV_PATH "ODBVer"

type upload_method = 
  | OCamlForge
  | Uscan
  | Manual of user
  | API of user
  with sexp

type t = 
  {
    pkg:           string;
    ver:           version;
    order:         int;
    tarball:       filename;
    upload_date:   date;
    upload_method: upload_method;
    publink:       url;
    oasis:         bool;
  } with sexp 

type vt = 
  V1 of t with sexp 

(** Upgrade versions *)
let upgrade ~ctxt = 
  function
    | V1 t -> return t

(** Load from file *)
let from_file =
  LwtExt.IO.sexp_load vt_of_sexp upgrade

(** Dump to file *)
let to_file =
  LwtExt.IO.sexp_dump sexp_of_vt 
