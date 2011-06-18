
(** Manage version uploads
    @author Sylvain Le Gall
  *)

open ODBGettext
open ODBPkgVer
open ODBCompletion
open ODBUpload
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open Template
open Context
open Common

(** Internal state of the upload
  *)
type 'a t = 
  | Begin of ([`Begin], 'a ODBUpload.t) Task.t
  | Edit of 'a ODBUpload.t * LogBox.t
  | Commit of 'a ODBUpload.t * ([`Commit], ODBPkgVer.t) Task.t
  | Cancel of ([`Cancel], unit) Task.t


(** Table to store upload's states in session
  *)
let upload_data = 
  create_volatile_table ()

(** Set a state for a session/upload 
  *)
let upload_data_set ~sp id t = 
  let hsh = 
    match get_volatile_session_data ~table:upload_data ~sp () with 
      | Data hsh -> 
          hsh

      | Data_session_expired | No_data ->
          let hsh = 
            Hashtbl.create 2
          in
            set_volatile_session_data ~table:upload_data ~sp hsh;
            hsh
  in
    Hashtbl.replace hsh id t 

(** Unset a state for a session/upload 
  *)
let upload_data_unset ~sp id =
  match get_volatile_session_data ~table:upload_data ~sp () with 
    | Data hsh -> 
        Hashtbl.remove hsh id

    | Data_session_expired | No_data ->
        ()

(** Retrieve a state for a session/upload 
  *)
let upload_data_get ~sp id = 
  match get_volatile_session_data ~table:upload_data ~sp () with 
    | Data hsh -> 
        Hashtbl.find hsh id

    | Data_session_expired | No_data ->
        raise Not_found

(** Template of the upload pages
  *)
let upload_template ~ctxt ~sp ?extra_headers ctnt = 
  template
    ~ctxt
    ~sp
    ~title:(OneTitle (s_ "Upload"))
    ~div_id:"upload"
    ?extra_headers
    ctnt

(*
 * Edit data of completion
 *)

let upload_completion_box ~ctxt ~sp id upload =

  let service =
    Eliom_predefmod.Action.register_new_post_coservice_for_session'
      ~sp
      ~name:"upload_completion_action" 
      ~post_params:(opt (string "publink") **
                    string "package" **
                    ExtParams.version "version" **
                    int "order" **
                    int "id")
      ~keep_get_na_params:false
      (fun sp _ (publink, (pkg, (version, (ord, id)))) ->
         Context.get_user ~sp ()
         >>= fun (ctxt, _) ->
         begin
           match upload_data_get ~sp id with
             | Edit (upload, log) ->
                 begin
                   let upload = 
                     {upload with 
                          publink = publink;
                          completion = {upload.completion with 
                                            ct_pkg = Sure pkg;
                                            ct_ver = Sure version;
                                            ct_ord = Sure ord}}
                   in
                     upload_data_set ~sp id (Edit (upload, log));
                     return ()
                 end

             | Begin _ | Commit _ | Cancel _ ->
                 fail StateTransitionNotAllowed
         end)
  in

  let has_oasis =
    upload.completion.ct_oasis <> None 
  in

  let form =
    post_form ~service ~sp
      (fun (publink_nm, (pkg_nm, (ver_nm, (ord_nm, id_nm)))) ->
         [p
            [pcdata (s_ "Tarball: ");
             pcdata upload.tarball_nm;
             br ();

             pcdata (s_ "Public link: ");
             string_input
               ~input_type:`Text
               ~name:publink_nm
               ~value:(match upload.publink with
                         | Some lnk -> lnk
                         | None -> "")
               ();
             br ();

             pcdata (s_ "Package: ");
             begin
               let value =
                 match upload.completion.ct_pkg with 
                   | Sure v | Unsure (_, v) -> v
                   | NotFound -> ""
               in
                 if has_oasis then 
                   span 
                     [pcdata value;
                      pcdata (s_ " (from _oasis)");
                      string_input
                        ~input_type:`Hidden
                        ~name:pkg_nm
                        ~value
                        ()]
                 else
                   string_input
                     ~input_type:`Text
                     ~name:pkg_nm
                     ~value
                     ()
             end;
             br ();

             pcdata (s_ "Version: ");
             begin
               let value = 
                 match upload.completion.ct_ver with
                   | Sure v | Unsure (_, v) -> v
                   | NotFound -> 
                       OASISVersion.version_of_string ""
               in
                 if has_oasis then
                   span
                     [pcdata (OASISVersion.string_of_version value);
                      pcdata (s_ " (from _oasis)");
                      user_type_input
                        ~input_type:`Hidden
                        ~name:ver_nm
                        ~value
                        OASISVersion.string_of_version
                        ()]
                 else
                   user_type_input
                     ~input_type:`Text
                     ~name:ver_nm
                     ~value
                     OASISVersion.string_of_version
                     ()
             end;
             br ();

             pcdata (s_ "Order: ");
             int_input
               ~input_type:`Text
               ~name:ord_nm
               ~value:(match upload.completion.ct_ord with
                         | Sure v | Unsure (_, v) -> v
                         | NotFound -> 0)
               ();
             br ();
             
             int_input ~input_type:`Hidden ~name:id_nm ~value:id ();
             raw_input ~input_type:`Reset ~value:(s_ "Reset") ();
             string_input ~input_type:`Submit ~value:(s_ "Save") ();
            ]
         ])
      ()
  in

  let msg_lst = 
    ODBPkgVer.check (pkg_ver_of_upload upload)
  in

  let is_ok = 
    not (List.exists (function (`Error, _) -> true | _ -> false) msg_lst)
  in

  let content = 
    let to_html (lvl, msg) = 
      match lvl with 
        | `Error -> html_error [pcdata msg]
        | _ -> pcdata msg
    in
      match msg_lst with 
        | hd :: tl ->
            div 
              [ul
                 (li [to_html hd]) 
                 (List.map (fun e -> li [to_html e]) tl);
               form]
        | [] ->
            form 
  in

    return (is_ok, content)

(** Preview of the package version page
  *)
let upload_preview_box ~ctxt ~sp id upload = 
  catch 
    (fun () ->
       upload_phantom_storage ~ctxt:ctxt.odb upload
       >>= fun (pkg_ver, stor) ->
       ODBStorage.PkgVer.oasis stor (`PkgVer pkg_ver)
       >>= fun oasis_opt ->
       begin
         let ctxt = 
           {(Context.anonymize ctxt) with stor = stor}
         in
           PkgVerView.preview_box ~ctxt ~sp
             pkg_ver 
             (fun () ->
                return 
                  (XHTML.M.a  
                     (* TODO: temporary service for upload *)
                     ~a:[a_href (uri_of_string "http://NOT_UPLOADED")]
                     [pcdata upload.tarball_nm; pcdata (s_ " (backup)")],
                   upload.tarball_nm))
             oasis_opt
           >|= fun ctnt ->
           true, ctnt
       end)
    (fun e ->
       return
         (false,
          [html_error
             [pcdata
                (Printf.sprintf 
                   (f_ "Unable to create the package: %s") 
                   (Printexc.to_string e))]]))
  >|= fun (is_ok, content) ->
  is_ok, ((h3 [pcdata (s_ "Preview")]) :: content)

(* 
 * Cancel/confirm uploads
 *)

let upload_confirm_action =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"upload_confirm_action" 
    ~post_params:(string "action" ** int "id")
    ~keep_get_na_params:false
    (fun sp id (action, id) ->
       Context.get_user ~sp () 
       >>= fun (ctxt, _) ->
       begin
         match upload_data_get ~sp id  with 
           | Edit (upload, _) ->
               begin
                 try 
                   let state = 
                     match action with 
                       | "cancel" ->
                           Cancel
                             (Task.create ~ctxt 
                                (fun ctxt ->
                                   upload_rollback ~ctxt:ctxt.odb upload))

                       | "confirm" ->
                           Commit
                             (upload,
                              Task.create ~ctxt
                                (fun ctxt ->
                                   upload_commit ~ctxt:ctxt.odb upload))

                       | str ->
                           failwith (Printf.sprintf (f_ "Unknow action '%s'") str)
                   in
                     upload_data_set ~sp id state;
                     return ()
                 
                 with e ->
                   fail e
               end

           | Begin _ | Commit _ | Cancel _ ->
               fail StateTransitionNotAllowed
       end)

let upload_confirm_box ~ctxt ~sp id is_ok upload = 
  return
    (post_form 
       ~service:upload_confirm_action
       ~sp
       (fun (action, id') ->
          [p 
             [
               int_input 
                 ~input_type:`Hidden
                 ~name:id'
                 ~value:id
                 ();
               string_button 
                 ~name:action
                 ~value:"cancel" 
                 [pcdata (s_ "Cancel upload")];
               string_button 
                 ~a:(if is_ok then [] else [a_disabled `Disabled])
                 ~name:action
                 ~value:"confirm" 
                 [pcdata (s_ "Confirm upload")];
             ]])
       ())

(** Gather all edition box 
  *)
let upload_edit_box ~ctxt ~sp id upload log = 
  upload_preview_box ~ctxt ~sp id upload
  >>= fun (is_ok_preview, preview_box) ->
  upload_completion_box ~ctxt ~sp id upload
  >>= fun (is_ok_completion, completion_box) ->
  upload_confirm_box ~ctxt ~sp id 
    (is_ok_preview && is_ok_completion) 
    upload 
  >>= fun action_box ->
  LogBox.log_box ~ctxt ~sp log
  >>= fun log_box ->
  begin
    return 
      ([log_box]
       @
       [h3 [pcdata "Results"];
        completion_box;
       ]
       @
       preview_box
       @
       [action_box;
        js_script 
          ~uri:(make_uri ~service:(static_dir sp) 
          ~sp 
          ["form-disabler.js"]) ();
       ])
  end

(* 
 * Initialization action: upload a tarball
 *)

let upload_init_check tarball_fd publink_opt = 
  try 
    let publink_opt = 
      match publink_opt with 
        | Some s ->
            begin
              match ExtString.String.strip s with
                | "" -> None
                | s  -> Some s
                (* TODO: test the link *)
            end
        | None ->
            None
    in
    let tarball_nm =  
      match get_original_filename tarball_fd with 
        | "none" ->
            failwith 
              (s_ "No tarball uploaded, be sure to choose a \
                   file to upload.")
        | fn ->
            FilePath.basename fn 
    in
      LwtExt.IO.with_file_content (get_tmp_filename tarball_fd)
      >>= fun tarball_content ->
      return (tarball_content, tarball_nm, publink_opt)
  with e -> 
    fail e

let upload_init_action = 
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"upload_tarball"
    ~post_params:(string "publink" ** 
                  file "tarball" **
                  int "id")
    (fun sp _ (publink, (tarball_fd, id))->
       Context.get_user ~sp () 
       >>= fun (ctxt, accnt) ->
       upload_init_check tarball_fd (Some publink)
       >>= fun (tarball_content, tarball_nm, publink) ->
       begin
         let tsk = 
           Task.create 
             ~ctxt 
             (fun ctxt ->
                upload_begin ~ctxt:ctxt.odb
                  ctxt.stor
                  (Web accnt.OCAAccount.accnt_real_name)
                  tarball_content tarball_nm publink)
         in
           upload_data_set ~sp id (Begin tsk);
           return ()
       end)

let upload_init_box ~ctxt ~sp id =
  let upload_form = 
    post_form 
      ~service:upload_init_action
      ~sp
      (fun (publink, (tarball, id_nm)) ->
           [p [pcdata (s_ "Tarball: ");
               file_input 
                 ~a:[a_accept "application/x-bzip2;\
                               application/zip;\
                               application/x-gzip"]
                 ~name:tarball ();
               br ();
               pcdata (s_ "Public link: ");
               string_input ~input_type:`Text ~name:publink ~value:"" ();
               br ();
               int_input ~input_type:`Hidden ~name:id_nm ~value:id ();
               string_input ~input_type:`Submit ~value:(s_ "Upload") ()
           ]])
      ()
  in
    return [upload_form]

(** Service to handle state transition and related display/redirection
  *)
let upload_with_id =
  Eliom_predefmod.Any.register_new_service
    ~path:["upload"]
    ~get_params:(int "id")
    (fun sp id () ->
       Context.get_user ~sp () 
       >>= fun (ctxt, _) ->
       try 
         match upload_data_get ~sp id with 
           | Begin task_upload ->
               begin
                 Task.wait ~ctxt 
                   task_upload
                   ctxt.upload_delay
                   (s_ "The tarball is not yet processed.")
                 >>= fun (task, delay, upload) ->
                 begin
                   let log = 
                     Task.get_logger task 
                   in
                   let () = 
                     upload_data_set ~sp id (Edit (upload, log))
                   in
                     upload_edit_box ~ctxt ~sp id upload log
                     >>= fun edit_box ->
                     upload_template ~ctxt ~sp
                       ((p                   
                           [pcdata 
                              (Printf.sprintf
                                 (f_ "The tarball '%s' has been processed \
                                      in %.3fs, please check and complete \
                                      the result.")
                                 upload.tarball_nm delay)])
                       :: edit_box)
                     >>= fun page ->
                     Eliom_predefmod.Xhtml.send ~sp page
                 end
               end

           | Edit (upload, log) ->
               begin
                 upload_edit_box ~ctxt ~sp id upload log
                 >>= fun edit_box ->
                 upload_template ~ctxt ~sp edit_box
                 >>= fun page -> 
                 Eliom_predefmod.Xhtml.send ~sp page
               end

           | Commit (upload, task_commit) ->
               begin
                 catch 
                   (fun () -> 
                      Task.wait ~ctxt 
                        task_commit
                        ctxt.upload_commit_delay
                        (s_ "Commit of upload not yet finished.")
                      >>= fun (task, delay, pkg_ver) ->
                      begin
                        upload_data_unset ~sp id;
                        Eliom_predefmod.Redirection.send ~sp 
                          (preapply 
                             view_pkg_ver
                             (pkg_ver.ODBPkgVer.pkg, 
                              Version pkg_ver.ODBPkgVer.ver))
                      end)
                   (fun e ->
                      let ctxt = 
                        Task.set_logger task_commit ctxt
                      in
                        ODBMessage.error ~ctxt:ctxt.odb 
                          (f_ "Unable to commit upload: %s")
                          (Printexc.to_string e)
                        >>= fun () ->
                        begin
                          let log = 
                            Task.get_logger task_commit
                          in
                          let () =
                            upload_data_set ~sp id 
                              (Edit (upload, log))
                          in
                            upload_edit_box ~ctxt ~sp id upload log
                            >>= fun edit_box ->
                            upload_template ~ctxt ~sp edit_box
                            >>= fun page ->
                            (* TODO: xhtml error ? *)
                            Eliom_predefmod.Xhtml.send ~sp page
                        end)
               end

           | Cancel task_cancel ->
               begin
                 Task.wait ~ctxt 
                   task_cancel
                   ctxt.upload_cancel_delay 
                   (s_ "Cancelation of upload not yet finished.")
                 >>= fun (task, delay, ()) ->
                 begin
                   upload_data_unset ~sp id;
                   Eliom_predefmod.Redirection.send ~sp upload
                 end
               end

       with Not_found ->
         begin
           (* Nothing is defined, we start a new upload *)
           upload_init_box ~ctxt ~sp id
           >>= fun init_box ->
           upload_template ~ctxt ~sp init_box
           >>= fun page ->
           Eliom_predefmod.Xhtml.send ~sp page
         end)

(** Upload counter 
  *)
let upload_id = 
  ref 1

(** Redirect to upload_with_id
  *)
let upload_handler sp () () = 
  incr upload_id;
  return (preapply upload_with_id !upload_id)
