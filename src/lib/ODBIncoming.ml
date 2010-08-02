
(*open Sexplib.Conv*)
open ODBMessage
open ODBGettext
open ODBTypes
open ODBInotify
open ODBCompletion
open ODBVer
open Sexplib.Conv
open Lwt

TYPE_CONV_PATH "ODBIncoming"

type upload_t =
  {
    publink:       url sexp_option;
    upload_method: upload_method;
  } with sexp 

type t =
  | Step1_JustUploaded of upload_t
  (** Use just uploaded the package *)
  | Step2_UserEditable of upload_t * ODBCompletion.t
  (** We run parameter completion but either we are not sure
      about values or there are errors or the upload method 
      implies a user validation.
   *)
  | Step3_UserValidated of upload_t * name * version * int * filename option
  (** The user has reviewed the parameter, we can proceed.
   *)
  with sexp

type vt = 
  V1 of t with sexp

let upgrade ~ctxt =
  function
    | V1 t -> return t

(** Load from file *)
let from_file = 
  LwtExt.IO.sexp_load vt_of_sexp upgrade

(** Dump to file *)
let to_file =
  LwtExt.IO.sexp_dump sexp_of_vt (fun t -> V1 t)

let move_to_storage ~ctxt ut pkg ver ord tarball_fn sexp_fn oasis_fn =
  let upload_date =
    CalendarLib.Calendar.from_unixfloat
      (Unix.stat tarball_fn).Unix.st_mtime
  in
  let ver = 
    {
      ODBVer.pkg    = pkg;
      ver           = ver;
      ord           = ord;
      tarball       = Filename.basename tarball_fn;
      upload_date   = upload_date;
      upload_method = ut.upload_method;
      publink       = ut.publink;
    }
  in
    (* TODO: catch errors and avoid deletion *)
    ODBStorage.add_version ~ctxt ver tarball_fn oasis_fn
    >>= fun () ->
    ODBFileUtil.rm ~ctxt [tarball_fn; sexp_fn]

let upload_complete ~ctxt sexp_fn tarball_fn = 
  from_file ~ctxt sexp_fn
  >>= function
    | Step1_JustUploaded ut ->
        begin
          debug ~ctxt
            (f_ "Run completion on tarball '%s'")
            tarball_fn
          >>= fun () ->
          ODBArchive.uncompress_tmp_dir ~ctxt tarball_fn 
          (fun fn an dn ->
            ODBCompletion.run ~ctxt fn an dn 
            >>= fun ct ->
            (* Conditions to go to step 2 or directly 
             * to storage
             *)
            let upload_method_need_ack = 
              match ut.upload_method with
              | Manual _ -> true
              | OCamlForge | Uscan | API _ -> false
            in

              match ct with 
              (* Completion is sure *)
              | {ODBCompletion.pkg = Sure pkg; 
                 ver = Sure ver; ord = Sure ord;
                 oasis_fn = oasis_fn} 
                 when not upload_method_need_ack ->
                  debug ~ctxt 
                    (f_ "Everything is complete, move tarball to storege")
                  >>= fun () -> 
                  (* We have everything we need -> move to storage *)
                  move_to_storage ~ctxt ut pkg ver ord 
                    tarball_fn sexp_fn oasis_fn

              | _ ->
                  (* We need some inputs from user -> go to step 2 *)
                  to_file ~ctxt sexp_fn (Step2_UserEditable (ut, ct)))
        end

    | Step2_UserEditable _ ->
        debug ~ctxt 
          (f_ "Wait for user input")

    | Step3_UserValidated (ut, pkg, ver, ord, oasis_fn) ->
        debug ~ctxt
          (f_ "Moving tarball to storage")
        >>= fun () ->
        move_to_storage ~ctxt ut pkg ver ord
          tarball_fn sexp_fn oasis_fn

module SetString = Set.Make(String)

let wait_complete ~ctxt ev changed = 
  match ev with 
  | Created fn ->
      begin
        return changed
      end
  
  | Changed fn ->
      begin
        let changed = SetString.add fn changed in

        let sexp_fn, tarball_fn  = 
          if Filename.check_suffix fn ".sexp" then
            fn, Filename.chop_extension fn
          else
            fn^".sexp", fn
        in

        (* Check that we have received Changed event for 
         * both tarball and sexp 
         *)
        if SetString.mem sexp_fn changed && 
           Sys.file_exists sexp_fn &&
           SetString.mem tarball_fn changed && 
           Sys.file_exists tarball_fn then
          (* We have a winner, the upload seems complete *)
          let ctxt = 
            ODBContext.sub ctxt (Filename.basename tarball_fn)
          in
          debug 
            ~ctxt
            (f_ "Upload complete for file '%s'")
            tarball_fn
          >>= fun () ->
          upload_complete ~ctxt sexp_fn tarball_fn 
          >>= fun () ->
          return changed

        else
          return changed

          (*
      else if sexp_exists then
        debug ~ctxt
          (f_ "Missing file '%s'") sexp_fn
      else if tarball_exists then
        debug ~ctxt
          (f_ "Missing file '%s'") tarball_fn
      else
        debug ~ctxt
          (f_ "Missing file '%s' and '%s'")
          sexp_fn tarball_fn
          *)
      end

  | Deleted fn ->
      begin
        return (SetString.remove fn changed)
      end

let run = 
  ODBRunner.singleton 
    "ODBIncoming.run"
    (fun ~ctxt () ->
      let ctxt = ODBContext.sub ctxt "incoming" in

      ODBInotify.monitor_dir ~ctxt 
        (wait_complete ~ctxt)
        ODBConf.incoming_dir SetString.empty
      >>= fun changed ->
      if SetString.cardinal changed > 0 then
        info ~ctxt
          (f_ "Remaining files in the incoming directory: %s")
            (String.concat (s_ ", ")
              (List.rev_map 
                (fun fn -> 
                  Printf.sprintf
                    (if Sys.file_exists fn then
                      format_of_string "%s"
                    else
                      "%s?")
                    (Filename.basename fn))
                (SetString.elements changed)))
        else
          return ())

let make upload_method = 
  Step1_JustUploaded {publink = None; upload_method = upload_method}
