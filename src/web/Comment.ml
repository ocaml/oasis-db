
(** Web services to display/register comments per package
    @author Sylvain Le Gall
  *)

open Lwt
open XHTML.M
open ODBGettext

let pkg_ver_box ~sp ~ctxt pkg =
  return 
    (div ~a:[a_class ["comment"]]
       [pcdata "TODO Comment"])

let pkg_box ~sp ~ctxt pkg_ver =
  return 
    (div ~a:[a_class ["comment"]]
       [pcdata "TODO Comment"])
