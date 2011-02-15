
(** Web services to display/register ratings per package
    @author Sylvain Le Gall
  *)

open Lwt
open XHTML.M
open ODBGettext


let pkg_box ~sp ~ctxt pkg = 
  return 
    (span [pcdata (s_ "Rating: 3 stars (TODO)")])

let pkg_ver_box ~sp ~ctxt pkg_ver = 
  pkg_box ~sp ~ctxt pkg_ver.ODBPkgVer.pkg
  >|= fun res ->
  (res, res)

