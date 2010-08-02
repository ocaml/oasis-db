
open TestCommon
open OUnit
open Unix
open FileUtil
open ODBVer
open CalendarLib

let default_timeout = 1.0

let wait_create ~is_file ?(timeout=default_timeout) topfn = 

  let directory_exists dn =
    Sys.file_exists dn && Sys.is_directory dn
  in

  let fd = 
    Inotify.init ()
  in

  let end_time =
    (Unix.gettimeofday ()) +. timeout
  in

  let existence_test ~is_file fn =
    (* Maybe the file already exists? *)
    (is_file && Sys.file_exists fn) ||
    (* Maybe the directory already exists? *)
    (not is_file && directory_exists fn)
  in

  let loop_test ~is_file fn =
    let dn =
      Filename.dirname fn
    in
    let wd = 
      Inotify.add_watch fd dn
        (if is_file then
           [Inotify.S_Close_write]
         else
           [Inotify.S_Create])
    in
    let rec loop_select () = 
      let timeout =
        end_time -. (Unix.gettimeofday ())
      in
        if timeout > 0.0 then
          begin
                 
              if existence_test ~is_file fn then 
                true
              else
                let _, _, _ =
                  Unix.select [fd] [] [] timeout
                in
                  loop_select () 
          end
        else
          (* We reach the timeout *)
          false
    in
    let res = 
      loop_select ()
    in
      Inotify.rm_watch fd wd;
      res
  in

  (* Find the first existing directory *)
  let rec wait_aux ~is_file fn =
    if FilePath.is_current fn then
      begin
        failwith 
          (Printf.sprintf 
             "Cannot find an existing directory to watch for file '%s'"
             topfn)
      end 
    else 
      begin
        let dn = 
          Filename.dirname fn
        in
          if directory_exists dn then
            (* Directory exists, test for its content *)
            loop_test ~is_file fn 

          else if wait_aux ~is_file:false dn then
            (* Directory doesn't exist, wait for its creation 
             * and retest.
             *)
            wait_aux ~is_file fn

          else
            (* Unable to create toplevel directory... *)
            false
      end
  in

    wait_aux ~is_file topfn


let wait_remove ?(timeout=default_timeout) fn =
  let fd =
    Inotify.init ()
  in
    try 
      let wd = 
        Inotify.add_watch fd fn [Inotify.S_Delete_self]
      in
      let res =
        match Unix.select [fd] [] [] timeout with 
          | _ :: _, _, _ -> 
              true

          | [], _, _ ->
              false
      in
        Inotify.rm_watch fd wd;
        res

    with 
      | Inotify.Error ("add_watch", 2) ->
          (* ENOENT *)
          true

let wait_change ?(timeout=default_timeout) fn f =
  let fd =
    Inotify.init ()
  in
  let wd = 
    Inotify.add_watch fd fn [Inotify.S_Delete_self]
  in

  let end_timeout = 
    (Unix.gettimeofday ()) +. timeout
  in

  let gtd () = 
    end_timeout -. (Unix.gettimeofday ())
  in

  let rec aux () = 
    match Unix.select [fd] [] [] (gtd ()) with 
      | _ :: _, _, _ -> 
          if f () then
            true 
          else
            aux ()

      | [], _, _ ->
          false
  in

  let res =
    aux ()
  in
    Inotify.rm_watch fd wd;
    res

let assert_create_file ?timeout fn =
  assert_bool
    (Printf.sprintf "File '%s' created" fn)
    (wait_create ?timeout ~is_file:true fn)

let assert_create_dir ?timeout fn =
  assert_bool
    (Printf.sprintf "Directory '%s' created" fn)
    (wait_create ?timeout ~is_file:false fn)

let assert_remove_file ?timeout fn =
  assert_bool
    (Printf.sprintf "File '%s' removed" fn)
    (wait_remove ?timeout fn)

let assert_remove_dir ?timeout fn =
  assert_bool
    (Printf.sprintf "Directory '%s' removed" fn)
    (wait_remove ?timeout fn)

let assert_changed ?timeout ~what fn f = 
  assert_bool
    (Printf.sprintf "File '%s' doesn't match %s" fn what) 
    (wait_change ?timeout fn f)

let in_data_dir fn = 
  FilePath.make_filename ["test"; "data"; fn]

let in_incoming_dir fn = 
  FilePath.concat ODBConf.incoming_dir fn 

let in_dist_dir fn =
  FilePath.concat ODBConf.dist_dir fn

let odb_run ctxt f = 
  Lwt_main.run (f ~ctxt:ctxt.ctxt ())

let assert_ver_file ctxt pkg ver =
  let mk_fn bn =
    in_dist_dir (FilePath.make_filename [pkg; ver; bn])
  in
  let () =
    assert_create_file (mk_fn "storage.sexp");
    assert_create_file (mk_fn "_oasis");
    assert_create_file (mk_fn "_oasis.pristine")
  in
  let ver =
    odb_run ctxt 
      (fun ~ctxt () -> 
         ODBVer.from_file 
           ~ctxt 
           (mk_fn "storage.sexp"))
  in
    assert_create_file (mk_fn ver.tarball)


let upload ctxt mthd time fn = 
  let fn_sexp = 
    Filename.concat 
    ODBConf.incoming_dir 
    ((Filename.basename fn)^".sexp")
  in
    cp [in_data_dir fn] ODBConf.incoming_dir;
    touch ~time (in_incoming_dir (Filename.basename fn));
    odb_run
      ctxt
      (fun ~ctxt () ->
         ODBIncoming.to_file
           ~ctxt
           fn_sexp
           (ODBIncoming.make mthd))

let tests ctxt = 
  "Incoming" >::
  (fun () ->
    (* Start the process FakeIncoming *)
    let pid = 
      create_process 
        ctxt.fake_incoming [|ctxt.fake_incoming|]
        stdin stdout stderr
    in
    let clean_exit () = 
      (* End scenario *)
      kill pid Sys.sigterm;
      snd (waitpid [] pid)
    in
    begin
      try 
        rm ~recurse:true [ODBConf.incoming_dir; ODBConf.dist_dir];
        mkdir ODBConf.incoming_dir;
        mkdir ODBConf.dist_dir;
        List.iter 
          (fun (mthd, fn, date, pkg, ver) -> 
             let tm = 
               Calendar.to_unixfloat
                 (Printer.Calendar.from_string date)
             in
             upload ctxt mthd (Touch_timestamp tm) fn;
             assert_ver_file ctxt pkg ver)
          [
            OCamlForge, "baz_3.O~alpha1.zip", 
            "2010-07-31 15:35:09",
            "baz", "3.0~alpha1";

            Uscan, "foo-0.1.0.tar.gz", 
            "2010-07-30 16:00:00",
            "foo", "0.1.0";

            OCamlForge, "ocaml-csvgenerator-0.0.5.tar.gz", 
            "2010-07-29 14:00:00",
            "ocaml-csvgenerator", "0.0.5";

            OCamlForge, "ocaml-data-notation-0.0.1.tar.gz", 
            "2010-07-28 12:00:00",
            "ocaml-data-notation", "0.0.1";

            OCamlForge, "ocaml-fastrandom-0.0.1.tar.gz", 
            "2010-06-07 13:00:00",
            "ocaml-fastrandom", "0.0.1";

            OCamlForge, "ocamlify-0.0.1.tar.gz", 
            "2010-06-01 08:30:00",
            "ocamlify", "0.0.1";

            OCamlForge, "ocaml-moifile-0.1.0.tar.gz", 
            "2010-05-14 14:30:00",
            "ocaml-moifile", "0.1.0";

            OCamlForge, "ocaml-moifile-0.1.1.tar.gz", 
            "2010-05-03 17:00:00",
            "ocaml-moifile", "0.1.1";

            OCamlForge, "ocaml-posix-resource-0.0.1.tar.gz", 
            "2010-04-16 10:30:00",
            "ocaml-posix-resource", "0.0.1";

            OCamlForge, "oasis-0.2.0~alpha1.tar.gz", 
            "2010-04-12 09:25:12",
            "oasis", "0.2.0~alpha1";

            OCamlForge, "oasis-0.1.0.tar.gz", 
            "2010-04-11 13:35:02",
            "oasis", "0.1.0";
          ];

        upload ctxt (Manual "gildor") Touch_now "bar-0.2.0.tar.bz2";
        (* TODO: check and provide extra data for bar *)

      with e ->
        ignore (clean_exit ());
        raise e
    end;

    assert_equal 
      (WSIGNALED Sys.sigterm)
      (clean_exit ()))
