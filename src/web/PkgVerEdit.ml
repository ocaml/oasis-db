
(** Edit package's version 
   
    @author Sylvain Le Gall
  *)

open OASISVersion
open ODBGettext
open Template
open Context
open XHTML.M
open OASISUtils
open ODBPkgVer 
open Eliom_parameters
open Eliom_services
open Eliom_predefmod.Xhtml
open Common
open Lwt

let edit_pkg_ver_box ~ctxt sp (pkg_str, ver) = 

  let action =
    Eliom_predefmod.Action.register_new_post_coservice_for_session'
      ~sp
      ~name:"edit_pkg_ver_action" 
      ~post_params:(opt (string "publink"))
      (fun sp _ publink -> 
         (* TODO: modify the storage in a memory copy *)
         return ())
  in

  let display () = 
    ODBStorage.PkgVer.find ctxt.stor (`StrVer (pkg_str, ver))
    >>= fun pkg_ver ->
    ODBStorage.PkgVer.oasis ctxt.stor (`PkgVer pkg_ver)
    >|= fun oasis_opt ->
    let has_oasis = 
      oasis_opt <> None
    in
    let form =
      post_form 
        ~service:action
        ~sp
        (fun publink_nm ->
           [p 
              [pcdata (s_ "Tarball: "); 
               pcdata pkg_ver.tarball; 
               br ();

               pcdata (s_ "Has _oasis file: ");
               pcdata (if has_oasis then s_ "true" else s_ "false");
               br ();

               pcdata (s_ "Public link: ");
               string_input
                 ~input_type:`Text
                 ~name:publink_nm
                 ~value:(match pkg_ver.publink with
                           | Some lnk -> lnk
                           | None -> "")
                 ();
               br ();

               pcdata (s_ "Package: ");
               pcdata pkg_ver.pkg;
               br ();

               pcdata (s_ "Version: ");
               pcdata (string_of_version pkg_ver.ver);
               br ();

               pcdata (s_ "Order: ");
               pcdata (string_of_int pkg_ver.ord);
               br ();

               raw_input ~input_type:`Reset ~value:(s_ "Reset") ();
               string_input ~input_type:`Submit ~value:(s_ "Save") ();
              ]])
        ()
    in
      div 
        [h3 [pcdata (s_ "General information")]; form]
  in

    BoxedForms.create display



let edit_oasis_box ~ctxt sp (pkg_str, ver) = 

  let action =
    Eliom_predefmod.Action.register_new_post_coservice_for_session'
      ~sp
      ~name:"edit_oasis_action" 
      ~post_params:(string "oasis")
      (fun sp _ oasis_str ->
         (* TODO: modify in memory storage *)
         return ())
  in

  let display () = 
    ODBStorage.PkgVer.find ctxt.stor (`StrVer (pkg_str, ver))
    >>= fun pkg_ver ->
    ODBStorage.PkgVer.with_file_in ctxt.stor (`PkgVer pkg_ver) `OASIS
      ~catch:(function 
                | ODBStorage.FileDoesntExist ->
                    return ""
                | e ->
                    fail e)
      (fun chn  ->
         LwtExt.IO.with_file_content_chn ~fn:"_oasis" chn)
    >|= fun oasis_str ->
    let form = 
      post_form
        ~service:action
        ~sp
        (fun oasis_nm ->
           [p
              [textarea
                 ~name:oasis_nm
                 ~value:oasis_str
                 ~rows:40
                 ~cols:80
                 ();
               br ();

               raw_input ~input_type:`Reset ~value:(s_ "Reset") ();
               string_input ~input_type:`Submit ~value:(s_ "Save") ();
              ]])
        ()
    in
      div
        [h3 [pcdata (s_ "_oasis file")]; form]
  in

    BoxedForms.create display


let start_edit ~ctxt sp ((pkg_str, ver) as k) edited =
  let edit_pkg_ver_box = 
    edit_pkg_ver_box ~ctxt sp k
  in

  let edit_oasis_box = 
    edit_oasis_box ~ctxt sp k
  in

  let action = 
    Eliom_predefmod.Action.register_new_post_coservice_for_session
      ~fallback:Common.view_pkg_ver
      ~sp
      ~post_params:(string "action")
      (fun sp _ action ->
         begin
           let lst = 
             [edit_pkg_ver_box; 
              edit_oasis_box]
           in
             match action with 
               | "cancel" ->
                   BoxedForms.rollbacks () lst

               | "confirm" ->
                   BoxedForms.commits () lst

               | _ ->
                   fail 
                     (Failure 
                        (Printf.sprintf 
                           (f_ "Unknown action '%s'")
                           action))
         end
         >|= fun () ->
         Hashtbl.remove edited k)
  in

  let action_box = 
    post_form
      ~service:action
      ~sp
      (fun action_nm ->
         [p 
            [string_button 
               ~name:action_nm 
               ~value:"cancel" 
               [pcdata (s_ "Cancel")];
             string_button 
               ~name:action_nm 
               ~value:"confirm"
               [pcdata (s_ "Confirm")]]])
      (pkg_str, Version ver) 
  in

  let preview_box ~ctxt () = 
    (* TODO: move this to PkgVerView *)
    catch 
      (fun () ->
         ODBStorage.PkgVer.find ctxt.stor (`StrVer (pkg_str, ver))
         >>= fun pkg_ver ->
         ODBStorage.PkgVer.oasis ctxt.stor (`PkgVer pkg_ver)
         >>= fun oasis_opt -> 
         PkgVerView.box ~ctxt:(Context.anonymize ctxt) ~sp
           pkg_ver 
           (fun () ->
              return 
                (XHTML.M.a  
                   (* TODO: temporary service for upload *)
                   ~a:[a_href (uri_of_string "http://NOT_UPLOADED")]
                   [pcdata pkg_ver.tarball; pcdata (s_ " (backup)")],
                 pkg_ver.tarball))
           oasis_opt
         >|= fun content ->
           (h3 [pcdata (s_ "Preview")])
           ::
           content)
      (fun e ->
         return 
           [h3 [pcdata (s_ "Preview")];
            p [pcdata (s_ "Not enough data to build a preview")]])
      >|= fun lst ->
      div lst
  in

  let fpage () = 
    ODBStorage.PkgVer.find ctxt.stor (`StrVer (pkg_str, ver))
    >>= fun pkg_ver ->
    BoxedForms.display edit_pkg_ver_box ()
    >>= fun edit_pkg_ver_box ->
    BoxedForms.display edit_oasis_box ()
    >>= fun edit_oasis_box ->
    preview_box ~ctxt () 
    >>= fun preview_box ->
    begin
      let browser_ttl = 
        Printf.sprintf (f_ "%s v%s")
          pkg_str (string_of_version ver)
      in
      let page_ttl =
        Printf.sprintf (f_ "Edit %s v%s")
          pkg_str (string_of_version ver)
      in
        template
          ~ctxt
          ~sp
          ~title:(BrowserAndPageTitle (browser_ttl, page_ttl))
          ~div_id:"pkg_ver_edit"
          [
            edit_pkg_ver_box;
            edit_oasis_box;
            preview_box;
            action_box;

            js_script 
              ~uri:(make_uri ~service:(static_dir sp) 
              ~sp 
              ["form-disabler.js"]) ();
          ]
    end
  in

    Hashtbl.add edited k fpage;
    fpage () 

let rec handler sp (pkg_str, ver) () = 
    Context.get_user ~sp ()
    >>= fun (ctxt, accnt) ->
    begin
      let edited = 
        Hashtbl.create 13
      in

        (* This new service will override future call to the service *)
        Eliom_predefmod.Xhtml.register_for_session 
          ~sp
          ~service:Common.edit_pkg_ver
          (fun sp k _ ->
             try
               let fpage = 
                 Hashtbl.find edited k 
               in
                 fpage ()
             with Not_found ->
               start_edit ~ctxt sp k edited);

        (* First time, display the page *)
        start_edit ~ctxt sp (pkg_str, ver) edited
    end

