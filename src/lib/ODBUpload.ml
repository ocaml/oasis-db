
(** Upload procedure, to upload a new package version

    @author Sylvain Le Gall
  *)

open ODBTypes
open ODBCompletion
open ODBPkgVer
open ODBGettext
open Lwt

type t = 
    {
      mutable tarball_closed: bool;
      (* Is tarball_fd closed *)

      tarball_fd: Unix.file_descr;
      (* Handle to the tarball *)

      tarball_nm: filename;
      (* File name of the tarball *)

      publink: url option;
      (* Public URL *)

      completion: ODBCompletion.t;
      (* Completion result *)

      upload_date: date;
      (* Date of upload *)

      upload_method: ODBPkgVer.upload_method;
      (* Method of upload *)

      storage: ODBStorage.rw_t;
      (* Where the package's version will be stored *)
    }

let safe_clean t = 
  if not t.tarball_closed then
    begin
      try 
        prerr_endline ("Close fd "^t.tarball_nm);
        Unix.close t.tarball_fd;
        t.tarball_closed <- true;
      with _ ->
        prerr_endline ("Error while closing "^t.tarball_nm)
    end
  else
    begin
      prerr_endline ("Fd "^t.tarball_nm^" already closed");
    end

let pkg_ver_of_upload t = 
  let value_of_answer =
    function 
      | Sure vl | Unsure (_, vl) -> 
          vl
      | NotFound -> 
          raise Not_found
  in
    {
      pkg = value_of_answer t.completion.ct_pkg;
      ver = value_of_answer t.completion.ct_ver;
      ord = value_of_answer t.completion.ct_ord;
      ODBPkgVer.tarball = t.tarball_nm;
      upload_date       = t.upload_date;
      upload_method     = t.upload_method;
      publink           = t.publink;
    }

let check_exists ~ctxt ~assume_sure t = 
  try 

    let pkg_ver = 
      pkg_ver_of_upload t
    in
    let is_sure = 
      function
        | Sure _ -> true
        | Unsure _ | NotFound -> false
    in
    let sure = 
      assume_sure ||
      (
        (is_sure t.completion.ct_pkg)
        &&
        (is_sure t.completion.ct_ver)
      )
    in
      ODBStorage.PkgVer.mem t.storage (`PkgVer pkg_ver) 
      >>= function
        | true ->
            let msg = 
              Printf.sprintf
                (f_ "Package's version %s v%s already exists.")
                pkg_ver.pkg 
                (OASISVersion.string_of_version pkg_ver.ver)
            in
              if sure then 
                begin
                  fail (Failure msg)
                end
              else
                begin
                  ODBMessage.error ~ctxt "%s" msg
                  >>= fun () ->
                  return t
                end

        | false ->
            return t

  with Not_found ->
    return t


let upload_begin ~ctxt (stor: ODBStorage.rw_t) upload_method tarball_fn tarball_nm publink =  
  let upload_date =
    CalendarLib.Calendar.from_unixfloat
      (Unix.stat tarball_fn).Unix.st_mtime
  in
  let tmp_fn, tarball_fd = 
    let tmp_dir = 
      Filename.dirname tarball_fn
    in
    let rec find_fn idx cnt = 
      if cnt = 0 then
        begin
          failwith 
            (Printf.sprintf 
               (f_ "Unable to find a temporary name for tarball in '%s'")
               tmp_dir)
        end
      else
        begin
          let fn = 
            Filename.concat
              tmp_dir
              (Printf.sprintf "tarball-%06d" idx)
          in
            try 
              let fd = 
                Unix.link tarball_fn fn;
                Unix.openfile fn [Unix.O_RDONLY] 0o640
              in
                (* On Unix if we remove an open file, we still have access
                 * to data, but the file will get removed when the fd will
                 * be closed -> this is a temporary file.
                 *)
                Unix.unlink fn;
                fn, fd
            with _ ->
              find_fn (idx + 1) (cnt - 1)
        end
    in
      find_fn (Random.bits ()) 100
  in
    ODBMessage.debug ~ctxt (f_ "Hardlink uploaded tarball '%s' ('%s') to '%s'")
      tarball_nm tarball_fn tmp_fn
    >>= fun () ->
    ODBArchive.uncompress_tmp_dir ~ctxt tarball_fd tarball_nm 
    (fun nm an dn ->
      ODBCompletion.run ~ctxt stor nm an dn)
    >>= fun ct ->
    begin
      let t = 
        {
          tarball_closed = false;
          tarball_fd     = tarball_fd;
          tarball_nm     = tarball_nm;
          publink        = publink;
          completion     = ct;
          upload_date    = upload_date;
          upload_method  = upload_method;
          storage        = stor;
        }
      in
        Gc.finalise safe_clean t;
        check_exists ~ctxt ~assume_sure:false t
    end

let upload_commit ~ctxt t = 
  check_exists ~ctxt ~assume_sure:true t
  >>= fun t ->
  begin
    try 
      return (pkg_ver_of_upload t)
    with e ->
      fail e 
  end
  >>= fun pkg_ver ->
  (* Inject the newly created package version into the storage *)
  ODBStorage.Pkg.mem t.storage (`PkgVer pkg_ver)
  >>= 
  begin
    function 
      | true ->
          return []
      | false ->
          ODBStorage.Pkg.create 
            ~ctxt 
            t.storage 
            {ODBPkg.pkg_name  = pkg_ver.ODBPkgVer.pkg;
             ODBPkg.pkg_watch = None}
          >>= fun (timestamp, ev, _) ->
          return [timestamp, ev]
  end 
  >>= fun evs ->

  ODBStorage.PkgVer.create ~ctxt t.storage pkg_ver t.tarball_fd
  >>= fun (timestamp, ev, pkg_ver) ->
  return ((timestamp, ev) :: evs)
  >>= fun evs ->

  (* Create _oasis and _oasis.pristine *)
  begin
    match t.completion.ct_oasis with 
      | Some str ->
          begin
            let dump fn = 
              ODBStorage.PkgVer.with_file_out
                t.storage
                (`PkgVer pkg_ver)
                fn
                (fun chn ->
                   Lwt_io.write chn str)
            in
              Lwt.join 
                [dump `OASIS; 
                 dump `OASISPristine]
          end

      | None ->
          return ()
  end

  (* Clean environment *)
  >>= fun () ->
  begin
    safe_clean t;
    return (List.rev evs, pkg_ver)
  end

let upload_rollback ~ctxt t = 
  safe_clean t;
  return ()

