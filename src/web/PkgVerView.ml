
(** Web services to display package version
    @author Sylvain Le Gall
  *)

open Lwt
open XHTML.M
open Eliom_predefmod.Xhtml
open OASISVersion
open OASISTypes
open ODBGettext
open ODBPkgVer
open Common
open Context
open Template

let page ~ctxt ~sp pkg_ver = 
  (* Load OASIS file *)
  ODBStorage.PkgVer.oasis ctxt.stor (`PkgVer pkg_ver) 
  >>= fun oasis_opt ->

  begin
    let backup_link () = 
      (* Backup download link *)
      Dist.a_dist 
        ~sp ~ctxt pkg_ver 
        (fun fn -> 
           [pcdata 
              (Printf.sprintf 
                 (f_ "%s (backup)") 
                 (FilePath.basename fn))])
        `Tarball
    in
      catch 
        (fun () ->
           PkgVerViewCommon.box ~ctxt ~sp pkg_ver backup_link oasis_opt)
        (fun e ->
           return
             [html_error
                [pcdata 
                   (Printf.sprintf
                      (f_ "An error occured while creating the \
                           view of the package: %s")
                      (Printexc.to_string e))]])
  end
  >>= fun box ->
  Monitor.box ~sp ~ctxt (`PkgVer pkg_ver)
  >>= fun monitor_box ->
  Rating.pkg_ver_box ~sp ~ctxt pkg_ver
  >>= fun (_, rating_box) ->
  PkgVerDerive.box ~sp ~ctxt pkg_ver
  >>= fun derive_box ->
  PkgVerRemove.box ~sp ~ctxt pkg_ver
  >>= fun remove_box ->
  PkgVerEdit.box ~sp ~ctxt pkg_ver
  >>= fun edit_box ->
  begin


    (* Page titles *)
    let browser_ttl =
      Printf.sprintf (f_ "%s v%s") 
        pkg_ver.pkg 
        (string_of_version pkg_ver.ver)
    in
    let page_ttl = 
      match oasis_opt with 
        | Some {synopsis = s} -> 
            Printf.sprintf (f_ "%s: %s") pkg_ver.pkg s
        | None -> 
            pkg_ver.pkg
    in
      template 
        ~ctxt
        ~sp 
        ~title:(BrowserAndPageTitle (browser_ttl, page_ttl)) 
        ~div_id:"browse" 
        [
          div box;

          div 
            ~a:[a_class ["action"]]
            [
              ul 
                (li 
                   [a view_pkg_ver sp 
                      [pcdata (s_ "Package page")] 
                      (pkg_ver.pkg, NoVersion)])
                [li [monitor_box];
                 li [edit_box];
                 li [derive_box];
                 li [remove_box];
                 li [rating_box]]
            ]
        ]
  end

let view_handler sp (pkg, ver_opt) () =
  Context.get ~sp () 
  >>= fun ctxt ->
  catch 
    (fun () ->
       match pkg, ver_opt with 
         | pkg_str, NoVersion ->
             ODBStorage.Pkg.find ctxt.stor (`Str pkg_str)
             >>= 
             PkgView.package_page ~ctxt ~sp 

         | pkg_str, Version ver ->
             ODBStorage.PkgVer.find ctxt.stor (`StrVer (pkg_str, ver))
             >>= 
             page ~ctxt ~sp 

         | pkg_str, LatestVersion ->
             ODBStorage.PkgVer.latest ctxt.stor (`Str pkg_str)
             >>= 
             page ~ctxt ~sp)
    (function
       | Not_found ->
           fail Eliom_common.Eliom_404
       | e ->
           fail e)
