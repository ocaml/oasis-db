
(** Display a package 
  
    @author Sylvain Le Gall
  *)

open Eliom_predefmod.Xhtml
open Eliom_parameters
open XHTML.M
open ODBGettext
open Template
open Lwt
open ODBPkg
open Context

let package_box ~ctxt ~sp pkg = 
  ODBStorage.PkgVer.elements ctxt.stor (`Pkg pkg)
  >>= fun pkg_ver_lst ->
  catch 
    (fun () ->
       ODBStorage.PkgVer.latest ctxt.stor (`Pkg pkg)
       >|= fun v ->
       Some v)
    (function 
       | Not_found ->
           return None
       | e ->
           fail e)
  >>= fun pkg_ver_latest_opt ->
  Comment.pkg_box ~sp ~ctxt pkg.pkg_name
  >|= fun comment_box ->
  begin
   [
     h3 [pcdata "Uscan"];

     begin
       match pkg.pkg_watch with 
         | Some str ->
             pre [pcdata str];
         | None ->
             em [pcdata (s_ "No watch file")];
     end;

     h3 [pcdata "Versions"];

     begin
       match pkg_ver_latest_opt with 
         | Some pkg_ver_latest ->
             p (Common.versions_field ~sp 
                  pkg_ver_lst None pkg_ver_latest)
         | None -> 
             p [pcdata (s_ "none")]
     end;

     comment_box;
   ]
  end

let package_page ~ctxt ~sp pkg =  
  package_box ~ctxt ~sp pkg
  >>= fun box ->
  begin
    let ttl = 
      Printf.sprintf (f_ "Package %s") pkg.pkg_name
    in
      template ~ctxt ~sp
        ~title:(OneTitle ttl) 
        ~div_id:"package" 
        box
  end
