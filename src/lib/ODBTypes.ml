
(** General types
  
   @author Sylvain Le Gall
 *)

open Sexplib.Conv

(**/**)
let string_sexp = string_of_sexp, sexp_of_string
(**/**)

(** OASIS-DB user *)
type user = string

let user_of_sexp, sexp_of_user = string_sexp

(** URL *)
type url = string

let url_of_sexp, sexp_of_url = string_sexp


(** Version *)
type version = OASISVersion.t

let version_of_sexp s = 
  OASISVersion.version_of_string (string_of_sexp s)

let sexp_of_version v = 
  sexp_of_string (OASISVersion.string_of_version v)

(** Filename *)
type filename = string

let filename_of_sexp, sexp_of_filename = string_sexp

(** Data (ISO) *)
type date = CalendarLib.Calendar.t

let date_of_sexp s = 
  CalendarLib.Printer.Calendar.from_string (string_of_sexp s)

let sexp_of_date d =
  sexp_of_string (CalendarLib.Printer.Calendar.to_string d)


