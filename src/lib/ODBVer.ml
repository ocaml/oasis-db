
(** Package's version 
 
    @author Sylvain Le Gall
  *)

open Lwt
open ODBTypes
open ODBGettext

TYPE_CONV_PATH "ODBVer"

type upload_method = 
  | OCamlForge
  | Uscan
  | Manual of user
  | API of user
  with sexp


let string_of_upload_method = 
  function 
    | OCamlForge -> 
        "forge.ocamlcore.org"

    | Uscan -> 
        "uscan"

    | Manual user -> 
        Printf.sprintf
          (f_ "web by %s") user

    | API user ->
        Printf.sprintf
          (f_ "API by %s") user


type t = 
  {
    pkg:           string;
    ver:           version;
    ord:           int;
    tarball:       filename;
    upload_date:   date;
    upload_method: upload_method;
    publink:       url option;
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
  LwtExt.IO.sexp_dump sexp_of_vt (fun t -> V1 t)

let compare t1 t2 = 
  match t1.ord - t2.ord with 
    | 0 ->
        OASISVersion.version_compare t1.ver t2.ver
    | n ->
        n
