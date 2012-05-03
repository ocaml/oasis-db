
open ODBContext
open OUnit

let verbose = ref true

let odb = 
  let rebase fn = 
    FilePath.make_filename ["test"; "data"; "storage"; fn]
  in
    ref 
      (ODBContext.default (rebase "incoming"))

let ocsigen = ref "ocsigen"
let ocsigen_args = ref ["-s"]

let oasis_cli = ref "false"

let in_data_dir fn = 
  FilePath.make_filename ["test"; "data"; fn]

let odb_run f = 
  Lwt_main.run (f ~ctxt:!odb ())

let long =
  ref true

let skip_long () =
  skip_if (not !long)
    "Skipping long test"

open Unix

(** Check that localhost port is free for listening *)
let is_port_free n =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let res = 
    setsockopt sock SO_REUSEADDR true;
    try 
      bind sock (ADDR_INET (inet_addr_loopback, n));
      true
    with e -> 
      false
  in
    close sock;
    res

let () = 
  Random.self_init ()

type ocsigen_t =
    {
      ocs_port:     int;
      ocs_addr:     inet_addr;
      ocs_rootdir:  string;
      ocs_command:  string;
      ocs_base_url: string;
    }

let bracket_tmpdir f = 
  bracket
    (fun () ->
       let dn =
         Filename.temp_file "oasis-db-" ".dir"
       in
         FileUtil.rm [dn];
         FileUtil.mkdir dn;
         dn)
    f
    (fun dn ->
       FileUtil.rm ~recurse:true [dn])

module InotifyExt =
struct 
  let default_timeout = 10.0

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

    let res = 
      wait_aux ~is_file topfn
    in
      Unix.close fd;
      res


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
          Unix.close fd;
          res

      with 
        | Inotify.Error ("add_watch", 2) ->
            (* ENOENT *)
            Unix.close fd;
            true

  let wait_change ?(timeout=default_timeout) fn f =
    let fd =
      Inotify.init ()
    in
    let wd = 
      Inotify.add_watch fd fn [Inotify.S_Close_write]
    in

    let end_timeout = 
      (Unix.gettimeofday ()) +. timeout
    in

    let gtd () = 
      end_timeout -. (Unix.gettimeofday ())
    in

    let rec aux () = 
      if f () then
        begin
          true
        end
      else
        begin
          match Unix.select [fd] [] [] (gtd ()) with 
            | _ :: _, _, _ -> 
                aux ()
                  
            | [], _, _ ->
                false
        end
    in

    let res =
      aux ()
    in
      Inotify.rm_watch fd wd;
      Unix.close fd;
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
end

let conf_std str ocs fn = 
   let std_conf = 
     Printf.sprintf 
       "<port>%s:%d</port>
        <charset>utf-8</charset>
        <logdir>%s</logdir>
        <commandpipe>%s</commandpipe>
        <extension findlib-package=\"ocsigen.ext.staticmod\"/>
        <extension findlib-package=\"ocsigen.ext.ocsipersist-sqlite\">
          <database file=\"%s/ocsidb\"/>
        </extension>
        <extension findlib-package=\"ocsigen.ext.eliom\"/>
        <uploaddir>%s</uploaddir>
        <maxuploadfilesize>4MB</maxuploadfilesize>"
       (string_of_inet_addr ocs.ocs_addr)
       ocs.ocs_port ocs.ocs_rootdir ocs.ocs_command
       ocs.ocs_rootdir ocs.ocs_rootdir
   in
   let buf = Buffer.create (String.length str + String.length std_conf) in
   let () =
     Buffer.add_substitute
       buf
       (function
          | "port" -> 
              (string_of_inet_addr ocs.ocs_addr)^":"
              ^(string_of_int ocs.ocs_port)
          | "rootdir" -> ocs.ocs_rootdir
          | "curdir"  -> FileUtil.pwd ()
          | "std_conf" -> std_conf
          | "command_fn" -> ocs.ocs_command
          | str ->
              invalid_arg
                (Printf.sprintf "conf_std(%s)" str))
       str
   in
   let chn = open_out fn in
     Buffer.output_buffer chn buf;
     close_out chn

let conf_oasis_db ocs fn =
  let args = 
    List.fold_left
      (fun acc (vr, vl) -> "-set" :: vr :: vl :: acc)
      ["etc/ocsigen.conf.in"; "-o"; fn]
      [
        "port", (string_of_inet_addr ocs.ocs_addr)^":"
          ^(string_of_int ocs.ocs_port);
        "topdir", Sys.getcwd (); 
        "command_pipe", ocs.ocs_command;
        "logdir", ocs.ocs_rootdir;
        "ocsidb", Filename.concat ocs.ocs_rootdir "ocsidb";
        "upload_dir", ocs.ocs_rootdir;
        "incoming_dir", Filename.concat ocs.ocs_rootdir "incoming";
        "dist_dir", Filename.concat ocs.ocs_rootdir "dist";
        "db_fn", Filename.concat ocs.ocs_rootdir "db.sql";
      ]
  in
  let args = "-local" :: args in
    assert_command "ocaml" ("etc/generate.ml" :: args)

let bracket_ocsigen conf pre_start f post_stop () =

  let port =
    (* Find an available port *)
    let rec find port = 
      if is_port_free port then
        port
      else
        find (port + 1)
    in
      find ((Random.int 8000) + 1025)
  in

  let addr = 
    inet_addr_loopback 
  in

  let rootdir = 
    let fn = 
      Filename.temp_file "oasis-db-" ".dir"
    in
      FileUtil.rm [fn];
      FileUtil.mkdir fn;
      fn
  in

  let command_fn = 
    Filename.concat rootdir "ocsigen_command"
  in

  let conf_fn = Filename.concat rootdir "ocsigen.conf" in

  let dbug fmt =
    if !verbose then
      begin
        Printf.fprintf Pervasives.stderr "DEBUG: ";
        Printf.kfprintf 
          (fun chn -> Printf.fprintf chn "\n%!")
          Pervasives.stderr
          fmt
      end
    else
      begin
        Printf.ifprintf 
          Pervasives.stderr 
          fmt
      end
  in

  let shutdown pid error_code = 
    let check_status = 
      function  
        | _, Unix.WEXITED code ->
            OUnit.assert_equal 
              ~msg:"Error code"
              error_code
              code

        | _, _ ->
            OUnit.assert_failure 
              (Printf.sprintf 
                 "Ocsigen process %d exited with error"
                 pid)
    in

    (* Still running, ask to exit *)
    match Unix.waitpid [Unix.WNOHANG] pid with 
      | 0, _ ->
          begin
            if Sys.file_exists command_fn then
              begin
                (* Command pipe available *)
                let chn = open_out command_fn in
                  dbug "Trying graceful exit for ocsigen process %d" pid;
                  output_string chn "shutdown\n";
                  close_out chn
              end
            else
              begin
                dbug "Killing ocsigen process %d" pid;
                Unix.handle_unix_error 
                  (Unix.kill pid) Sys.sigterm
              end;
            check_status (Unix.waitpid [] pid)
          end

      | res ->
          check_status res
  in

  let clean () = 
    FileUtil.rm ~recurse:true [rootdir] 
  in

  let ocs = 
    {
      ocs_port =  port;
      ocs_addr =  addr;
      ocs_rootdir = rootdir;
      ocs_command = command_fn;
      ocs_base_url = 
        Printf.sprintf "http://%s:%d/" 
          (string_of_inet_addr inet_addr_loopback) port;
    }
  in

    try 
      begin
        let () = 
          (* Create configuration file and log dir *)
          conf ocs conf_fn;
          if !verbose then
            begin
              let chn = open_in conf_fn in
                try 
                  while true do 
                    prerr_endline (input_line chn)
                  done
                with End_of_file ->
                  close_in chn
            end;
          pre_start ocs
        in

        (* Override LWT_LOG to start ocsigen *)
        let lwt_log_bak =
          let bak =
            try 
              Some (Unix.getenv "LWT_LOG")
            with Not_found ->
              None
          in
            if !verbose then
              Unix.putenv "LWT_LOG" "* -> debug";
            bak
        in

        (* Start ocsigen *)
        let pid = 
          create_process 
            !ocsigen 
            (Array.of_list 
               (!ocsigen 
                :: 
                (!ocsigen_args @ ["-c"; conf_fn])))
            Unix.stdin Unix.stdout Unix.stderr
        in

        let () = 
          InotifyExt.assert_create_file command_fn
        in

        (* Restore LWT_LOG *)
        let () =
          match lwt_log_bak with 
            | Some s -> Unix.putenv "LWT_LOG" s
            | None -> ()
        in

          (* Test *)
          begin
            try 
              f ocs;
              (* Stop ocisgen *)
              shutdown pid 0;
            with e ->
              shutdown pid 0;
              raise e
          end;

          post_stop ocs;
          clean ();
      end
    with e ->
      clean ();
      raise e

let bracket_oasis_db pre_start f post_stop = 
   bracket_ocsigen
     conf_oasis_db
     (fun ocs ->
        List.iter
          (fun nm ->
             FileUtil.mkdir 
               (Filename.concat ocs.ocs_rootdir nm))
          ["incoming"; "dist"];
        pre_start ocs)

     f post_stop

let in_incoming_dir ocs fn = 
  FilePath.make_filename [ocs.ocs_rootdir; "incoming"; fn]

let in_dist_dir ocs fn =
  FilePath.make_filename [ocs.ocs_rootdir; "dist"; fn]

