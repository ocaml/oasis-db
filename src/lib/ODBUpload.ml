
(** Upload procedure, to upload a new package version

    @author Sylvain Le Gall
  *)

open ODBTypes
open ODBCompletion
open ODBPkgVer
open ODBGettext
open Lwt

type 'a t = 
    {
      tarball_content: string;
      (* Content of the tarball *)

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

      storage: 'a ODBStorage.read_write;
      (* Where the package's version will be stored *)
    }

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


let upload_begin ~ctxt stor upload_method tarball_content tarball_nm publink =  
  let upload_date =
    CalendarLib.Calendar.now ()
  in
  let tarball_fn = 
    Filename.concat "tmp://" tarball_nm
  in
    begin
      try 
        let arch_handler =
          ODBArchive.of_filename tarball_nm 
        in
        let arch_name =
          ODBArchive.chop_suffix arch_handler tarball_nm 
        in
          FileUtilExt.with_temp_dir "oasis-db-upload-" ".dir"
            (fun dn ->
               LwtExt.IO.MemoryIn.with_file_in tarball_content
                 (fun chn -> 
                    ODBArchive.uncompress 
                      ~ctxt 
                      ~src:tarball_fn 
                      arch_handler
                      chn
                      dn)
                 >>= fun () ->
                 ODBCompletion.run ~ctxt stor tarball_nm arch_name dn)
      with e ->
        fail e
    end
    >>= fun ct ->
    begin
      let t = 
        {
          tarball_content = tarball_content;
          tarball_nm      = tarball_nm;
          publink         = publink;
          completion      = ct;
          upload_date     = upload_date;
          upload_method   = upload_method;
          storage         = stor;
        }
      in
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
          return ()
      | false ->
          ODBStorage.Pkg.create 
            ~ctxt 
            t.storage 
            {ODBPkg.pkg_name  = pkg_ver.ODBPkgVer.pkg;
             ODBPkg.pkg_watch = None}
          >>= fun pkg ->
          return ()
  end 
  >>= fun () ->

  LwtExt.IO.MemoryIn.with_file_in t.tarball_content
    (fun chn ->
       ODBStorage.PkgVer.create ~ctxt t.storage pkg_ver chn)
  >>= fun pkg_ver ->

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
  return pkg_ver

let upload_rollback ~ctxt t = 
  return ()

