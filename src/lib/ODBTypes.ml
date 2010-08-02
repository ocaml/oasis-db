
(** General types
  
   @author Sylvain Le Gall
 *)

open Sexplib.Conv

TYPE_CONV_PATH "ODBTypes"

(** Name like package name *)
type name = string with sexp

(** OASIS-DB user *)
type user = string with sexp 

(** URL *)
type url = string with sexp 

(** Filename *)
type filename = string with sexp 

(** Dirname *)
type dirname = string with sexp 

(** Data (ISO) *)
type date = CalendarLib.Calendar.t

let date_of_sexp s = 
  CalendarLib.Printer.Calendar.from_string (string_of_sexp s)

let sexp_of_date d =
  sexp_of_string (CalendarLib.Printer.Calendar.to_string d)

(** Version *)
type version = OASISVersion.t

let version_of_sexp s = 
  OASISVersion.version_of_string (string_of_sexp s)

let sexp_of_version v = 
  sexp_of_string (OASISVersion.string_of_version v)

