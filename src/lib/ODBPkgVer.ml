
(** Package's version 
 
    @author Sylvain Le Gall
  *)

open Lwt
open ODBTypes
open ODBGettext
open ODBMessage
open Sexplib.Conv

TYPE_CONV_PATH "ODBPkgVer"

type upload_method = 
  | OCamlForge
  | Uscan
  | Incoming of user
  | Web of user
  | WebAPI of user
  with sexp


let string_of_upload_method = 
  function 
    | OCamlForge -> 
        "forge.ocamlcore.org"

    | Uscan -> 
        "uscan"

    | Incoming user ->
        Printf.sprintf
          (f_ "incoming directory by %s") user

    | Web user -> 
        Printf.sprintf
          (f_ "web by %s") user

    | WebAPI user ->
        Printf.sprintf
          (f_ "Web API by %s") user


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

(** Load from file *)
let from_chn ~fn chn =
  LwtExt.IO.sexp_load_chn ~fn vt_of_sexp upgrade chn

(** Dump to file *)
let to_chn ~fn chn =
  LwtExt.IO.sexp_dump_chn ~fn sexp_of_vt (fun t -> V1 t) chn

let compare t1 t2 = 
  match t1.ord - t2.ord with 
    | 0 ->
        OASISVersion.version_compare t1.ver t2.ver
    | n ->
        n

let check t =
  List.fold_left
    (fun acc -> 
       function 
         | Some v -> v :: acc
         | None -> acc)
    []
    [
      if t.pkg = "" then
        Some (`Error, s_ "Empty package name")
      else
        None;

      if OASISVersion.string_of_version t.ver = "" then
        Some (`Error, s_"Empty version name")
      else
        None;

      if t.tarball = "" then 
        Some (`Error, s_ "Empty tarball name")
      else
        None;
    ]
