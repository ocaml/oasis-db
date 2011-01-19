
open ODBContext

let fake_incoming : string option ref = ref None
let verbose = ref true

let odb = 
  ref 
    (ODBContext.default 
       (FilePath.make_filename ["test"; "data"; "storage"]))

let ocsigen = ref "ocsigen"
let ocsigen_args = ref ["-s"]

let in_data_dir fn = 
  FilePath.make_filename ["test"; "data"; fn]

let in_incoming_dir fn = 
  FilePath.concat !odb.incoming_dir fn 

let in_dist_dir fn =
  FilePath.concat !odb.dist_dir fn

let odb_run f = 
  Lwt_main.run (f ~ctxt:!odb ())


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

let bracket_ocsigen conf pre_start f post_stop () =

  let conf_fn, chn = 
    Filename.open_temp_file "oasis-db-" ".conf"
  in

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
       <uploaddir>%s</uploaddir>"
      (string_of_inet_addr addr)
      port rootdir command_fn rootdir rootdir
  in

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
    FileUtil.rm ~recurse:true [conf_fn; rootdir]
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
        let buf = 
          Buffer.create (String.length conf)
        in
        let () = 
          (* Create configuration file and log dir *)
          Buffer.add_substitute
            buf
            (function
               | "port" -> (string_of_inet_addr addr)^":"^(string_of_int port)
               | "rootdir" -> rootdir
               | "curdir"  -> FileUtil.pwd () 
               | "std_conf" -> std_conf
               | "command_fn" -> command_fn
               | str -> 
                   invalid_arg 
                     (Printf.sprintf "with_ocsigen_running(%s)" str))
            conf;
          Buffer.output_buffer chn buf;
          close_out chn;
          if !verbose then
            begin
              Buffer.output_buffer Pervasives.stderr buf;
              Pervasives.flush Pervasives.stderr
            end;
          pre_start ocs
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
          (* TODO: we should be able to be sure that everything is initialized
           * here, without sleeping
           *)
          Unix.sleep 1
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



