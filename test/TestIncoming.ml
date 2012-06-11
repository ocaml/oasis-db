
open TestCommon
open TestCommon.InotifyExt
open OUnit
open Unix
open FileUtil
open CalendarLib
open ODBContext
open ODBPkgVer
open ODBIncoming

let assert_ver_file ocs pkg ver oasis tarball_org =
  let mk_fn bn =
    in_dist_dir ocs (FilePath.make_filename [pkg; ver; bn])
  in
  let ver =
    assert_create_file (mk_fn "storage.sexp");
    odb_run 
      (fun ~ctxt () -> 
         ODBPkgVer.from_file 
           ~ctxt 
           (mk_fn "storage.sexp"))
  in
  let tarball_fn =
    mk_fn ver.tarball
  in
    if !verbose then
      begin
        let dump_stat fn = 
          if Sys.file_exists fn then
            begin
              let st = stat fn in
                Printf.eprintf "File '%s' size %s, digest %s\n%!"
                  fn (string_of_size st.size)
                  (Digest.to_hex (Digest.file fn))
            end
          else
            Printf.eprintf "File '%s' doesn't exist\n%!" fn
        in
          dump_stat tarball_fn;
          dump_stat tarball_org
      end;
    assert_create_file tarball_fn;
    assert_changed ~what:tarball_org
      tarball_fn
      (fun () ->
         cmp tarball_fn tarball_org = None);
(*
    assert_bool 
      (Printf.sprintf 
         "Uploaded tarball '%s' and original tarball '%s' not equal\n%!"
         tarball_fn tarball_org)
      (cmp tarball_fn tarball_org = None);
 *)
    List.iter 
      (if oasis then 
         (fun nm -> 
            assert_create_file (mk_fn nm))
       else
         (fun nm -> 
            let fn = 
              mk_fn nm
            in
              assert_equal 
                ~msg:(Printf.sprintf "File '%s' don't exist" fn)
                ~printer:string_of_bool
                false
                (Sys.file_exists fn)))
      ["_oasis"; "_oasis.pristine"]

let upload ocs date fn = 
  let fn_sexp = 
    in_incoming_dir ocs (sexp_of_tarball fn) 
  in
  let fn_tarball = 
    in_incoming_dir ocs fn 
  in
  let tm = 
    Calendar.to_unixfloat
      (Printer.Calendar.from_string date)
  in
    if !verbose then
      Printf.eprintf "Copying file %s to %s\n%!" (in_data_dir fn) (fn_tarball);
    cp [in_data_dir fn] fn_tarball;
    touch ~time:(Touch_timestamp tm) fn_tarball;
    odb_run
      (fun ~ctxt () ->
         ODBIncoming.to_file ~ctxt fn_sexp {publink = None})

let tests = 
  let one ocs (fn, time, pkg, ver, oasis) () =
    upload ocs time fn;
    assert_ver_file ocs pkg ver oasis (in_data_dir fn)
  in

  let vecs = 
    [
      "baz_3.O~alpha1.zip", 
      "2010-07-31 15:35:09",
      "baz", "3.0~alpha1", 
      true;

      "foo-0.1.0.tar.gz", 
      "2010-07-30 16:00:00",
      "foo", "0.1.0",
      true;

      "ocaml-csvgenerator-0.0.5.tar.gz", 
      "2010-07-29 14:00:00",
      "ocaml-csvgenerator", "0.0.5",
      true;

      "ocaml-data-notation-0.0.1.tar.gz", 
      "2010-07-28 12:00:00",
      "ocaml-data-notation", "0.0.1",
      true;

      "ocaml-fastrandom-0.0.1.tar.gz", 
      "2010-06-07 13:00:00",
      "ocaml-fastrandom", "0.0.1",
      true;

      "ocamlify-0.0.1.tar.gz", 
      "2010-06-01 08:30:00",
      "ocamlify", "0.0.1",
      true;

      "ocaml-moifile-0.1.0.tar.gz", 
      "2010-05-14 14:30:00",
      "ocaml-moifile", "0.1.0",
      true;

      "ocaml-moifile-0.1.1.tar.gz", 
      "2010-05-03 17:00:00",
      "ocaml-moifile", "0.1.1",
      true;

      "ocaml-posix-resource-0.0.1.tar.gz", 
      "2010-04-16 10:30:00",
      "ocaml-posix-resource", "0.0.1",
      true;

      "oasis-0.2.0~alpha1.tar.gz", 
      "2010-04-12 09:25:12",
      "oasis", "0.2.0~alpha1",
      true;

      "oasis-0.1.0.tar.gz", 
      "2010-04-11 13:35:02",
      "oasis", "0.1.0",
      true;

      "bar-0.2.0.tar.bz2",
      "2010-04-11 13:35:02",
      "bar", "0.2.0", true;

      "mlblock-0.1.tar.bz2",
      "2008-04-25 23:49:02",
      "mlblock", "0.1", false;

      "hydro-0.7.1.tar.gz",
      "2010-04-11 13:35:02",
      "hydro", "0.7.1", false;

      "crypt-1.0.tar.gz",
      "2010-04-11 13:35:02",
      "crypt", "1.0", false;

      "ccss-1.0.tgz",
      "2010-03-10 16:58:02",
      "ccss", "1.0", false;

      "batteries-1.2.2.tar.gz",
      "2010-06-15 03:49:02",
      "batteries", "1.2.2", false;

      "batteries-1.2.1.tar.gz",
      "2010-06-12 15:33:02",
      "batteries", "1.2.1", false;

      "batteries-1.2.0.tar.gz",
      "2010-06-06 01:38:00",
      "batteries", "1.2.0", false;

      "batteries-1.1.0.tar.gz",
      "2010-02-28 14:26:02",
      "batteries", "1.1.0", false;

      "batteries-1.0.1.tar.gz",
      "2010-02-01 15:07:02",
      "batteries", "1.0.1", false;

      "batteries-1.0.0.tar.gz",
      "2010-01-15 19:57:00",
      "batteries", "1.0.0", false;
    ]
  in
    
    "Incoming" >:::
    (List.map 
       (fun ((fn, _, _, _, _) as vec) ->
          fn >:: 
          bracket_oasis_db
            (fun _ ->
               skip_long ())
            (fun ocs -> 
               one ocs vec ())
            ignore)
       vecs)
    @
    [
      "ManyInARow" >::
       (* Start an ocsigen process *)
       bracket_oasis_db
         (fun _ ->
            skip_long ())
         (fun ocs ->
            (* Auto upload *)
            List.iter 
              (fun vec -> 
                 one ocs vec ()) 
              vecs)
         ignore
    ]
