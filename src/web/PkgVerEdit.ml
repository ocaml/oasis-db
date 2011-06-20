
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
         let stor = ctxt.stor in
           Context.get_user ~sp () 
           >>= fun (ctxt, _) ->
           begin
             let ctxt = {ctxt with stor = stor} in
               ODBStorage.PkgVer.find ctxt.stor (`StrVer (pkg_str, ver))
               >>= fun pkg_ver ->
               ODBStorage.PkgVer.replace ctxt.stor 
                 (`PkgVer pkg_ver) 
                 {pkg_ver with publink = publink}
           end)
  in

  let display () = 
    ODBStorage.PkgVer.find ctxt.stor (`StrVer (pkg_str, ver))
    >>= fun pkg_ver ->
    ODBStorage.PkgVer.oasis ctxt.stor (`PkgVer pkg_ver)
    >|= fun oasis_opt ->
    begin
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

      let is_ok, content = 
        box_check
          (ODBPkgVer.check pkg_ver)
          form
      in
        is_ok,
        div 
          [h3 [pcdata (s_ "General information")]; 
           content]
    end
  in

    BoxedForms.create display


let edit_oasis_box ~ctxt sp is_new (pkg_str, ver) = 

  let oasis_org = 
    ref None
  in

  let action =
    Eliom_predefmod.Action.register_new_post_coservice_for_session'
      ~sp
      ~name:"edit_oasis_action" 
      ~post_params:(string "oasis")
      (fun sp _ oasis_str ->
         let stor = ctxt.stor in
           Context.get_user ~sp () 
           >>= fun (ctxt, _) ->
           begin
             let ctxt = {ctxt with stor = stor} in
               ODBStorage.PkgVer.with_file_out ctxt.stor (`StrVer (pkg_str, ver))
                 `OASIS
                 (fun chn ->
                    Lwt_io.write chn oasis_str)
           end)
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
         LwtExt.IO.with_file_content_chn chn)
    >|= fun oasis_str ->
    begin
      let () = 
        if !oasis_org = None && oasis_str <> "" then
          oasis_org := Some (ODBOASIS.from_string ~ctxt:ctxt.Context.odb oasis_str)
      in
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

      let msg_lst, oasis_opt = 
        try
          let tmp = ref [] in
          let oasis =
            ODBOASIS.from_string
              ~ctxt:ctxt.Context.odb
              ~printf:(fun lvl msg -> 
                         tmp := (lvl, msg) :: !tmp)
              oasis_str
          in
            List.rev !tmp,
            Some oasis 
        with e ->
          [`Error, (Printexc.to_string e)],
          None
      in

      let msg_lst =
        if is_new then
          begin
            (* Everything allowed on new package *)
            msg_lst
          end
        else
          begin
            match !oasis_org, oasis_opt with
              | Some oasis, Some oasis' ->
                  begin
                    let diff = 
                      ODBOASIS.check_build_sensitive oasis oasis' 
                    in
                      List.fold_left 
                        (fun acc (path, ev) ->
                           let msg = 
                             match ev with 
                               | `Changed (v1, v2) ->
                                   Printf.sprintf
                                     (f_ "Could not change %s from %s to %s in _oasis")
                                     path v1 v2
                               | `Missing ->
                                   Printf.sprintf
                                     (f_ "Could not remove field %s in _oasis")
                                     path
                               | `Added ->
                                   Printf.sprintf 
                                     (f_ "Could not add field %s in _oasis")
                                     path
                           in
                             (`Error, msg) :: acc)
                        msg_lst 
                        diff
                  end
              | None, Some _ ->
                  (`Error, (s_ "Could not add and _oasis file (derive a version)")) :: msg_lst
              | Some _, None ->
                  (`Error, (s_ "Could not remove the _oasis file")) :: msg_lst
              | None, None ->
                  msg_lst
          end
      in

      let is_ok, content = 
        box_check 
          msg_lst
          form
      in

        is_ok, 
        div
          [h3 [pcdata (s_ "_oasis file")]; 
           content]
    end
  in

    BoxedForms.create display 

let cp_diff fs1 fs2 = 
  ODBVFS.fold
    (fun ev () ->
       match ev with 
         | `PreDir fn ->
             begin
               fs2#file_exists fn
               >>= fun file_exists ->
               fs2#is_directory fn
               >>= fun is_directory ->
                 if file_exists && not is_directory then
                   fail 
                     (Failure
                        (Printf.sprintf
                           (f_ "Cannot replace file '%s' by directory '%s'")
                           (fs2#vroot fn) (fs1#vroot fn)))
                 else if not file_exists then
                   fs2#mkdir fn 0o755
                 else
                   return ()
             end
         | `PostDir _ -> 
             return ()
         | `File fn ->
             begin
               fs1#digest fn
               >>= fun digest1 ->
               fs2#file_exists fn 
               >>= fun file_exists ->
               fs2#is_directory fn 
               >>= fun is_directory ->
                 if file_exists && is_directory then
                   fail 
                     (Failure 
                        (Printf.sprintf 
                           (f_ "Cannot replace directory '%s' by file '%s'")
                           (fs2#vroot fn) (fs1#vroot fn)))
                 else if file_exists then
                   begin
                     fs2#digest fn
                     >>= fun digest2 ->
                       if digest1 <> digest2 then
                         fs2#cp (fs1 :> ODBVFS.read_only) [fn] fn
                       else
                         return ()
                   end
                 else 
                   begin
                     fs2#cp (fs1 :> ODBVFS.read_only) [fn] fn
                   end
             end)
    fs1 "" ()

let start_edit ~ctxt sp ((pkg_str, ver) as k) edited =
  let mem_fs = 
    new ODBFSMemory.read_write (ODBFSTree.root ())
  in
  let stor_fs = 
    ODBStorage.fs ctxt.stor
  in
    ODBFSMemory.cp_directories stor_fs mem_fs
    >>= fun () ->
    begin
      let phantom_fs = 
        new ODBVFSUnion.read_write mem_fs [stor_fs]
      in
        ODBStorage.create_read_write ~ctxt:ctxt.odb phantom_fs
    end
    >>= fun stor ->
    begin
      let ctxt = {ctxt with stor = stor} 
      in

      let edit_pkg_ver_box = 
        edit_pkg_ver_box ~ctxt sp k
      in

      let edit_oasis_box = 
        edit_oasis_box ~ctxt sp false k
      in

      let action = 
        Eliom_predefmod.Action.register_new_post_coservice_for_session
          ~fallback:Common.view_pkg_ver
          ~sp
          ~post_params:(string "action")
          (fun sp _ action ->
             Context.get_user ~sp () 
             >>= fun (ctxt, _) ->
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
                       >>= fun () ->
                       (* Copy the content of the filesystem to the real
                        * filesystem 
                        *)
                       cp_diff (mem_fs :> ODBVFS.read_only) stor_fs

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

      let action_box is_ok = 
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
                   ~a:(if is_ok then [] else [a_disabled `Disabled])
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
             PkgVerView.preview_box ~ctxt:(Context.anonymize ctxt) ~sp
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
               true, 
               (h3 [pcdata (s_ "Preview")])
               ::
               content)
          (fun e ->
             return 
               (false,
                [h3 [pcdata (s_ "Preview")];
                 p [pcdata (s_ "Not enough data to build a preview")]]))
          >|= fun (is_ok, lst) ->
          is_ok, div lst
      in

      let fpage () = 
        ODBStorage.PkgVer.find ctxt.stor (`StrVer (pkg_str, ver))
        >>= fun pkg_ver ->
        BoxedForms.display edit_pkg_ver_box ()
        >>= fun (is_ok_pkg_ver, edit_pkg_ver_box) ->
        BoxedForms.display edit_oasis_box ()
        >>= fun (is_ok_oasis, edit_oasis_box) ->
        preview_box ~ctxt () 
        >>= fun (is_ok_preview, preview_box) ->
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
                action_box (is_ok_pkg_ver && is_ok_oasis && is_ok_preview);

                js_script 
                  ~uri:(make_uri ~service:(static_dir sp) 
                  ~sp 
                  ["form-disabler.js"]) ();
              ]
        end
      in

        Hashtbl.add edited k fpage;
        fpage () 
    end

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

