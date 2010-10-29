
(** Manage version uploads
    @author Sylvain Le Gall
  *)

open ODBTypes
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
open Common

(* TODO: use session data + a state stored in session data *)

type t = 
    {
      temp_dir: dirname;
      (* Location of the temporary directory *)

      tarball: filename;
      (* File name of the tarball, inside the [temp_dir] *)

      mutable publink: url option;
      (* Public URL *)

      completion: ODBCompletion.t;
      (* Completion result *)

      upload_date: date;
      (* Date of upload *)

      upload_method: ODBVer.upload_method;
      (* Method of upload *)
    }

let uploads = 
  Task.create ()

let upload_task ~ctxt upload_method tarball_fn tarball publink = 
  let upload_date =
    CalendarLib.Calendar.from_unixfloat
      (Unix.stat tarball_fn).Unix.st_mtime
  in
  let f ~ctxt () = 
    (* Create a temporary directory *)
    ODBFileUtil.temp_dir ~ctxt:ctxt.odb "upload-" ".dir"
    >>= fun tmp_dn ->

    (* Move the uploaded tarball to temporary directory *)
    begin
      let tarball_tgt =
        Filename.concat tmp_dn tarball
      in
        ODBFileUtil.mv ~ctxt:ctxt.odb tarball_fn tarball_tgt
        >>= fun () ->
        return tarball_tgt
    end

    >>= fun tarball_fn ->
    ODBArchive.uncompress_tmp_dir ~ctxt:ctxt.odb tarball_fn 
    (fun fn an dn ->
      ODBCompletion.run ~ctxt:ctxt.odb fn an dn 
      >>= fun ct ->
      (* Move _oasis file out of the temporary directory *)
      begin
        match ct.oasis_fn with 
          | Some fn ->
              let tgt = 
                Filename.concat tmp_dn "_oasis"
              in
                ODBFileUtil.cp ~ctxt:ctxt.odb [fn] tgt
                >>= fun () ->
                return {ct with oasis_fn = Some tgt}
          | None ->
              return ct 
      end)

    >>= fun ct ->
      return
        {
          temp_dir      = tmp_dn;
          tarball       = Filename.basename tarball_fn;
          publink       = publink;
          completion    = ct;
          upload_date   = upload_date;
          upload_method = upload_method;
        }
    >>= fun res ->
    Lwt_unix.sleep 10.0
    >>= fun () ->
    return res

  in
    Task.add uploads f ~ctxt ()

let upload_template ~sp ?extra_headers ctnt = 
  auth_template
    ~sp
    ~title:(OneTitle (s_ "Upload"))
    ~div_id:"upload"
    ?extra_headers
    ()
  >>= fun (_, tmpl, _) ->
  tmpl ctnt

(*
 *
 * Step 3: Move the uploaded tarball and its data to storage 
 * or cancel upload.
 *
 *)

let upload_step3_no_post = 
  register_new_service 
    ~path:["upload_step3"]
    ~get_params:(int "id")
    (fun sp id _ ->
       Context.get_user ~sp ()
       >>= fun (ctxt, _) ->
       upload_template ~sp [p [pcdata "nothing"]])

(* 
 *  
 * Step 2: Wait for the tarball to be processed and display
 * parameters for validation once done.
 *  
 *)

let mk_ver t = 
  let value_of_answer =
    function 
      | Sure vl | Unsure (_, vl) -> 
          vl
      | NotFound -> 
          raise Not_found
  in
    {
      ODBVer.pkg    = value_of_answer t.completion.pkg;
      ver           = value_of_answer t.completion.ver;
      ord           = value_of_answer t.completion.ord;
      tarball       = t.tarball;
      upload_date   = t.upload_date;
      upload_method = t.upload_method;
      publink       = t.publink;
    }

let load_ver ~ctxt t = 
  catch 
    (fun () ->
       ODBVer.from_file ~ctxt:ctxt.odb 
         (ODBStorage.storage_filename t.temp_dir)
       >|= fun ver -> Some ver)
    (fun _ ->
       return None)

let upload_completion_box ~ctxt ~sp t = 
  let value_of_answer printer dflt vl = 
    match vl with 
      | Sure vl ->
          printer vl
      | Unsure (_, vl) ->
          printer vl
      | NotFound ->
          dflt
  in

  let tmpl_field ttl input =
    [
      pcdata ttl; input; br ();
    ]
  in

  let string_field_answer ttl nm printer dflt vl = 
    let vl = 
      value_of_answer printer dflt vl
    in
      tmpl_field 
        ttl 
        (string_input
           ~input_type:`Text
           ~name:nm ~value:vl ())
  in

  let int_field_answer ttl nm printer dflt vl = 
    let vl = 
      value_of_answer printer dflt vl
    in
      tmpl_field
        ttl
        (int_input
           ~input_type:`Text
           ~name:nm ~value:vl ())
  in
 
  let upload_update_storage =
    Eliom_predefmod.Any.register_new_post_coservice_for_session'
      ~name:"logoutpost" 
      ~post_params:(opt (string "publink") **
                    string "package" **
                    string "version" **
                    int "order")
      ~keep_get_na_params:false
      ~sp 
      (fun sp _ (publink, (pkg, (version, ord))) ->
         Context.get_user ~sp ()
         (* TODO: attach context to context task *)
         >>= fun (ctxt, _) ->
         load_ver ~ctxt t
         >>= fun ver_opt ->
         begin
           let ver = 
             match ver_opt with 
               | Some ver -> ver 
               | None -> mk_ver t
           in
           let ver' =
             {ver with 
                  ODBVer.pkg = pkg;
                  ver        = OASISVersion.version_of_string version;
                  ord        = ord;
                  publink    = publink}
           in
             Printf.eprintf "pkg: %s; ver: %s\n%!" pkg version;
             if check ~ctxt:ctxt.odb ver' then 
               ODBVer.to_file 
                 ~ctxt:ctxt.odb
                 (ODBStorage.storage_filename t.temp_dir)
                 ver'
             else 
               begin
                 return ()
               end
         end
         >>= fun () ->
         Eliom_predefmod.Redirection.send ~sp
           Eliom_services.void_hidden_coservice')
  in

    load_ver ~ctxt t
    >|= fun storage ->
    begin
      let has_oasis = 
        Sys.file_exists (Filename.concat t.temp_dir "_oasis")
      in
      let get fver vt = 
        match storage with 
          | Some v -> fver v
          | None -> vt
      in

        post_form 
          ~service:upload_update_storage 
          ~sp
          (fun (publink, (pkg, (ver, ord))) ->
             [p 
                (List.flatten
                   [tmpl_field
                      (s_ "Tarball: ")
                      (pcdata (get (fun v -> v.ODBVer.tarball) t.tarball));
                    tmpl_field
                      (s_ "Has _oasis file: ")
                      (pcdata 
                         (if has_oasis then
                            "false"
                          else
                            "true"));

                     tmpl_field
                      (s_ "Public link: ") 
                      (string_input
                         ~input_type:`Text
                         ~name:publink
                         ~value:(match get (fun v -> v.ODBVer.publink) t.publink with
                                   | Some lnk -> lnk
                                   | None -> "")
                         ());

                    string_field_answer
                      (s_ "Package: ") 
                      pkg 
                      (fun i -> i) 
                      ""
                      (get (fun v -> Sure v.ODBVer.pkg) t.completion.pkg);

                    string_field_answer
                      (s_ "Version: ") 
                      ver 
                      OASISVersion.string_of_version 
                      ""
                      (get (fun v -> Sure v.ODBVer.ver) t.completion.ver);

                    int_field_answer
                      (s_ "Order: ")
                      ord
                      (fun i -> i)
                      0
                      (get (fun v -> Sure v.ODBVer.ord) t.completion.ord);

                    [
                      string_input ~input_type:`Submit ~value:(s_ "Save") ();
                    ]
                   ]);
           ])
        ()
    end

let upload_preview_box ~ctxt ~sp t = 
  (* Test our ability to preview a valid storage datastructure *)
  load_ver ~ctxt t
  >>= 
  function 
    | Some ver ->
        begin
          Browse.mk_version_page' ~ctxt ~sp
            ver 
            (fun () ->
               return 
                 (XHTML.M.a  
                    (* TODO: temporary service for upload *)
                    ~a:[a_href (uri_of_string "http://NOT_UPLOADED")]
                    [pcdata t.tarball; pcdata (s_ " (backup)")],
                  "toto.tar.gz") (* TODO: real name *))
            (* TODO: replace by ODBStorage.Ver.... *)
            (return (Filename.concat t.temp_dir "_oasis"))
          >|= fun content ->
            (h3 [pcdata (s_ "Preview")])
            ::
            content
        end
    | None ->
        return 
          [h3 [pcdata (s_ "Preview")];
           p [pcdata (s_ "Not enough data to build a preview")]]


(* 
 * Cancel/confirm uploads
 *)

exception PageNoPost 

let upload_action_redirect_no_post =
  register_new_service
    ~path:["upload_action"]
    ~get_params:unit
    (fun _ _ _ ->
       fail PageNoPost)

let upload_action_redirect =
  Eliom_predefmod.Redirection.register_new_post_service 
    ~fallback:upload_action_redirect_no_post
    ~post_params:(string "action" ** int "id")
    (fun sp id (action, id) ->
       Context.get_user ~sp () 
       >>= fun (ctxt, _) ->
       Task.wait uploads ~ctxt id ctxt.upload_delay
       >>= fun (task, delay, res) ->
       begin
         match res with
           | Some t ->
               return (t, Task.set_logger task ctxt)
           | None ->
               fail (Failure "Timeout")
       end
       >>= fun (t, ctxt) ->
       begin
         match action with 
           | "cancel" ->
               ODBFileUtil.rm ~ctxt:ctxt.odb ~recurse:true [t.temp_dir]
               >>= fun () ->
               return upload 
           | "confirm" ->
               load_ver ~ctxt t
               >>= 
               begin
                 function
                   | Some ver ->
                       ODBStorage.Pkg.mem ver.ODBVer.pkg
                       >>= 
                       begin
                         function 
                           | true ->
                               return ()
                           | false ->
                               ODBStorage.Pkg.create ~ctxt:ctxt.odb ver.ODBVer.pkg
                               >>= 
                               ODBStorage.Pkg.move ~ctxt:ctxt.odb
                               >>= fun _ ->
                               return ()
                       end 
                       >>= fun () ->
                       ODBStorage.Ver.move ~ctxt:ctxt.odb t.temp_dir
                       >>= fun _ ->
                       return 
                         (preapply 
                            browse
                            (Some ver.ODBVer.pkg, 
                             Some (OASISVersion.string_of_version ver.ODBVer.ver)))
                   | None ->
                       (* TODO: redirect to upload page *)
                       fail (Failure (s_ "Invalid version"))
               end
           | str ->
               fail (Failure (Printf.sprintf (f_ "Unknow action '%s'") str))
       end)

let upload_action_box ~ctxt ~sp id t = 
  return
    (post_form 
       ~service:upload_action_redirect
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
                 ~name:action
                 ~value:"confirm" 
                 [pcdata (s_ "Confirm upload")];
             ]])
       ())

let upload_content ~ctxt ~sp id = 
  let tmpl ?extra_headers = 
    upload_template ~sp ?extra_headers
  in

  let html_log task = 
    let _, lst = 
      Queue.fold
        (fun (odd, acc) (sct, lvl, msg) ->
           let short_nm, css_style = 
             Log.html_log_level lvl 
           in
           let css_class = 
             [
               (if odd then "odd" else "even");
               css_style
             ]
           in
           let line =
             tr
               ~a:[a_class css_class]
               (td [pcdata short_nm])
               [td [pcdata msg]]
           in
             not odd, line :: acc)
        (true, [])
        (Task.get_logger task)
    in
      match lst with 
        | hd :: tl ->
          div 
            ~a:[a_class ["log"]]
            [h3 [pcdata (s_ "Log")];
             table hd tl]
        | [] ->
            pcdata ""
  in

    Task.wait uploads ~ctxt id ctxt.upload_delay
    >>= fun (task, delay, res) ->
    return (Task.set_logger task ctxt)
    >>= fun ctxt ->
    begin
      match res with 
        | None ->
            fail 
              (Timeout 
                 (Printf.sprintf 
                    (f_ "The tarball is not yet processed. \
                         This page will be refreshed in a few seconds.")))
                  (* TODO: allow to cancel upload *)

        | Some t ->
            return t
    end
    >>= fun t ->
    begin
      if not (Sys.file_exists (ODBStorage.storage_filename t.temp_dir)) then
        (* TODO: the test above is really not precise *)
        (* TODO: add this in the upload task *)
        (* We should init ASAP i.e. maybe when we can generate a valid ver *)
        try 
          let ver = 
            mk_ver t 
          in
            ODBStorage.Ver.init
              ~ctxt:ctxt.odb 
              ver 
              t.temp_dir
        with _ ->
          return ()
     else
       return ()
    end
    >>= fun () ->
    upload_preview_box ~ctxt ~sp t
    >>= fun preview_box ->
    upload_completion_box ~ctxt ~sp t
    >>= fun completion_box ->
    upload_action_box ~ctxt ~sp id t 
    >>= fun action_box ->
    begin
      return 
        ([h3 [pcdata "Results"];
          if delay >= 0.0 then 
            (* TODO: the test above is really not precise *)
            p                   
              [pcdata 
                 (Printf.sprintf
                    (f_ "The tarball '%s' has been processed in %.3fs, please \
                         check and complete the result.")
                    t.tarball delay)]
          else
              pcdata "";
          
          completion_box;
        ]
        @
         preview_box
        @
         [action_box])
    end
    >>= fun content -> 
    tmpl (html_log task :: content)

let upload_step2 =
  register_new_service
    ~path:["upload_step2"]
    ~get_params:(int "id")
    (fun sp id _ ->
       Context.get_user ~sp () 
       >>= fun (ctxt, _) ->
       upload_content ~ctxt ~sp id)

(*
 *
 * Step 1: upload a tarball
 *
 *)

let upload_step1_no_post = 
  Eliom_predefmod.Redirection.register_new_service
    ~path:["upload_step1"]
    ~get_params:unit
    (fun _ _ _ ->
       fail (Failure (s_ "Missing post parameters")))

let upload_step1 = 
  Eliom_predefmod.Redirection.register_new_post_service
    ~post_params:(string "publink" ** file "tarball")
    ~fallback:upload_step1_no_post
    (fun sp _ (publink, tarball_fd)->
       Context.get_user ~sp () 
       >>= fun (ctxt, _) ->
       begin
         let publink = 
           match ExtString.String.strip publink with
             | "" -> None
             | s  -> Some s
             (* TODO: test the link *)
         in
         let tarball =  
           (* TODO: test the tarball size > 0 *)
           FilePath.basename 
             (get_original_filename tarball_fd)
         in
         let tarball_fn =
           get_tmp_filename tarball_fd
         in
         let id = 
           upload_task ~ctxt 
             (Manual 
                (Account.name_of_role ctxt.role))
             tarball_fn tarball publink 
         in

           return (preapply upload_step2 id)
       end)

(* 
 *
 * Step 0: Main entry point, upload a tarball
 *
 *)

let upload_handler sp () () =
  let f = 
    post_form 
      ~service:upload_step1 
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
    upload_template ~sp [f]

