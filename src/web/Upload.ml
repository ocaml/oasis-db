
(** Manage version uploads
    @author Sylvain Le Gall
  *)

open ODBGettext
open ODBVer
open ODBIncoming
open ODBCompletion
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open Template
open Account
open Context

let upload_template ~sp ?extra_headers ctnt = 
  auth_template
    ~sp
    ~title:(OneTitle (s_ "Upload"))
    ~div_id:"upload"
    ?extra_headers
    ()
  >>= fun (_, tmpl, _) ->
  tmpl ctnt

let upload_anon_content ~sp () = 
  upload_template ~sp 
    [p ~a:[a_id "error"] 
       [pcdata (s_ "You must be login to upload")]]

let upload_anon =
  Defer.register_new_service 
    ~path:["upload_anon"]
    ~get_params:unit
    (fun sp () () ->
       upload_anon_content ~sp ())

(* Default action if we encounter a service without post parameter
 * go back to step 0, the upload form
 *)
let upload_step_no_post ~path =
  Defer.Redirection.register_new_service 
    ~path
    ~get_params:unit
    (fun sp () () -> return (upload ()))

(*
 *
 * Step 4: Wait for the file to be processed by ODBIncoming and appears
 * in the storage.
 *
 *)

let upload_step4 = 
  Defer.register_new_service 
    ~path:["upload_step4"]
    ~get_params:(string "tarball")
    (fun sp tarball () ->
       Context.get ~sp ()
       >>= fun ctxt ->
       check_step3 ~ctxt:ctxt.odb tarball
       >>= fun step3 ->
       begin
         let tmpl ?extra_headers = 
           upload_template ~sp ?extra_headers 
         in
           match step3 with 
             | Step3_NotYet ->
                 let refresh_delay =
                   5
                 in
                   tmpl
                     ~extra_headers:[meta 
                                       ~a:[a_http_equiv "refresh"] 
                                       ~content:(string_of_int refresh_delay)
                                      ()]
                     [p 
                        [pcdata 
                           (Printf.sprintf
                              (f_ "The tarball '%s' is not yet processed, \
                                   wait %d seconds.")
                              tarball
                              refresh_delay)]]

             | Step3_Back1 ->
                 tmpl [p [pcdata "TODO: back step1"]] 

             | Step3_Back2 ->
                 tmpl [p [pcdata "TODO: back step2"]] 

             | Step3_Finished ->
                 tmpl [p [pcdata "TODO: redirect to browse"]]

             | Step3_Bypassed ->
                 tmpl [p [pcdata "TODO: step2 bypassed"]]
       end)

(* 
 *
 * Step 3: Validate user parameters.
 *
 *)

let upload_step3_no_post = 
  upload_step_no_post ~path:["upload_step3"]

let upload_step3 = 
  Defer.Redirection.register_new_post_service
    ~post_params:(opt (string "publink") ** 
                  string "tarball" ** 
                  string "package" ** 
                  string "version" ** 
                  int "order" **
                  opt (string "oasis_fn"))
    ~fallback:upload_step3_no_post 
    (fun sp _ (publink, (tarball, (pkg, (ver, (ord, oasis_fn)))))->
       Context.get ~sp () 
       >>= fun ctxt ->
       let strip = 
         ExtString.String.strip 
       in
       let strip_opt = 
         function
           | Some s -> Some (strip s)
           | None -> None
       in

         match ctxt.role with 
           | User accnt | Admin accnt ->
               begin
                 ODBIncoming.validate
                   ~ctxt:ctxt.odb
                   (Manual accnt.accnt_name) 
                   (strip_opt publink)
                   (strip pkg)
                   (OASISVersion.version_of_string (strip ver))
                   ord
                   (strip_opt oasis_fn)
                   (strip tarball)
                 >>= fun () -> 
                 (* Wait a little bit, to let ODBIncoming catch and process the
                  * validation.
                  * TODO: rather than just waiting, watch inotify events also
                  *)
                 Lwt_unix.sleep 1.0
                 >>= fun () ->
                 return (preapply (upload_step4 ()) tarball)
               end 
           | Anon ->
               return (upload_anon ()))

(* 
 *  
 * Step 2: Wait for the file to be processed by ODBIncoming and that
 * the .sexp file matching the tarball contains results. Display 
 * parameters for validation once done.
 *  
 *)

let upload_step2 = 
  Defer.register_new_service
    ~path:["upload_step2"]
    ~get_params:(string "tarball")
    (fun sp tarball _ ->
       Context.get ~sp () 
       >>= fun ctxt ->
       check_step2 ~ctxt:ctxt.odb tarball
       >>= fun step2 ->
       begin
         let tmpl ?extra_headers = 
           upload_template ~sp ?extra_headers
         in
           match step2 with 
             | Step2_NotYet ->
                 let refresh_delay =
                   5
                 in
                   tmpl
                     ~extra_headers:[meta 
                                       ~a:[a_http_equiv "refresh"] 
                                       ~content:(string_of_int refresh_delay)
                                      ()]
                     [p 
                        [pcdata 
                           (Printf.sprintf
                              (f_ "The tarball '%s' is not yet processed, \
                                   wait %d seconds.")
                              tarball
                              refresh_delay)]]

             | Step2_Reached (ut, ct) ->
                 let field ttl read_only nm vl = 
                   [
                     pcdata ttl;
                     string_input 
                       ~input_type:`Text
                       ~a:(if read_only then 
                             [a_readonly `Readonly]
                           else 
                             [])
                       ~name:nm ~value:vl ();
                     br ();
                   ]
                 in

                 let field_sure ttl nm printer dflt vl =
                   match vl with 
                     | Sure vl ->
                         field ttl true nm (printer vl)
                     | Unsure (_, vl) ->
                         field ttl false nm (printer vl)
                     | NotFound ->
                         field ttl false nm dflt
                 in

                 let f = 
                   post_form 
                     ~service:(upload_step3 ())
                     ~sp
                     (fun (publink, (tarball_param, (pkg, (ver, (ord, oasis_fn))))) ->
                        [p 
                           (List.flatten
                              [field (s_ "Tarball: ") true tarball_param tarball;

                               begin
                                 match ut.publink with
                                   | Some lnk ->
                                       field (s_ "Public link: ") true publink lnk;
                                   | None ->
                                       []
                               end;

                               field_sure 
                                 (s_ "Package: ") 
                                 pkg 
                                 (fun i -> i) 
                                 ""
                                 ct.pkg;
                               field_sure (s_ "Version: ") 
                                 ver 
                                 OASISVersion.string_of_version 
                                 ""
                                 ct.ver;
                               [
                                 pcdata (s_ "Order: ");
                                 int_input
                                   ~input_type:`Text
                                   (* TODO: readonly *)
                                   ~name:ord
                                   (* TODO: real value *)
                                   ~value:0
                                   ()
                               ];

                               begin
                                 match ct.oasis_fn with 
                                   | Some fn -> 
                                       [string_input 
                                          ~input_type:`Hidden 
                                          ~name:oasis_fn 
                                          ~value:fn
                                          ()]
                                   | None ->
                                       []
                               end;

                               (* TODO: allow to cancel *)
                               (*string_button ~button_type:`Reset ~value:(s_ "Cancel") ();*)
                               [string_input ~input_type:`Submit ~value:(s_ "OK") ()]
                              ])])
                     ()
                 in
                 tmpl
                   [p 
                      [pcdata 
                         (Printf.sprintf
                            (f_ "The tarball '%s' has been processed, please \
                                 check and complete the result.")
                            tarball)];
                    f]

             | Step2_Bypassed ->
                 (* TODO: check if the file has appeared in the storage or not *)
                 tmpl
                   [p 
                      [pcdata
                         (Printf.sprintf 
                            (f_ "The tarball '%s' has been removed from the incoming \
                                 directory. Check for its presence in the archive.")
                            tarball)]]
       end)

(* 
 * 
 * Step 1: Upload the file 
 *
 *)

let upload_step1_no_post = 
  upload_step_no_post ~path:["upload_step1"]

let upload_step1 = 
  Defer.Redirection.register_new_post_service
    ~post_params:(string "publink" ** file "tarball")
    ~fallback:upload_step1_no_post
    (fun sp _ (publink, tarball_fd)->
       Context.get ~sp () 
       >>= fun ctxt ->
       match ctxt.role with 
         | Admin accnt | User accnt ->
             let t =
               let publink = 
                 match ExtString.String.strip publink with
                   | "" -> None
                   | s  -> Some s
               in
                 ODBIncoming.make ?publink (Manual accnt.accnt_name)
             in
             let tarball =  
               FilePath.basename (get_original_filename tarball_fd)
             in
               ODBIncoming.upload 
                 ~ctxt:ctxt.odb
                 ~tarball_fn:(get_tmp_filename tarball_fd)
                 t
                 tarball
               >>= fun () -> 
               (* Wait a little bit, to let ODBIncoming catch and process the
                * newly created file.
                * TODO: rather than just waiting, watch inotify events also
                *)
               Lwt_unix.sleep 1.0
               >>= fun () ->
               return (preapply (upload_step2 ()) tarball)
         | Anon ->
             return (upload_anon ()))

(* 
 *
 * Step 0: Main entry point, define the file to upload 
 *
 *)

let upload_handler =
  Defer.register 
    upload 
    (fun sp () () ->
       let f = 
         post_form 
           ~service:(upload_step1 ())
           ~sp
           (fun (publink, tarball) ->
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
                  string_input ~input_type:`Submit ~value:(s_ "Upload") ()
              ]])
           ()
       in
         upload_template ~sp [f])

let init () = 
  upload_handler ();
  ignore (upload_step4 ());
  ignore (upload_step3_no_post ());
  ignore (upload_step3 ());
  ignore (upload_step2 ());
  ignore (upload_step1_no_post ());
  ignore (upload_step1 ());
  ignore (upload_anon ())

