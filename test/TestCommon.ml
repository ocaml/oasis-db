
open ODBContext
open OUnit

let () =
  Printexc.register_printer 
    (function
       | Curl.CurlException (curl_code, http_code, str) ->
           let curl_str = 
             match curl_code with 
               |  Curl.CURLE_OK                          -> "CURLE_OK"
               |  Curl.CURLE_UNSUPPORTED_PROTOCOL        -> "CURLE_UNSUPPORTED_PROTOCOL"
               |  Curl.CURLE_FAILED_INIT                 -> "CURLE_FAILED_INIT"
               |  Curl.CURLE_URL_MALFORMAT               -> "CURLE_URL_MALFORMAT"
               |  Curl.CURLE_URL_MALFORMAT_USER          -> "CURLE_URL_MALFORMAT_USER"
               |  Curl.CURLE_COULDNT_RESOLVE_PROXY       -> "CURLE_COULDNT_RESOLVE_PROXY"
               |  Curl.CURLE_COULDNT_RESOLVE_HOST        -> "CURLE_COULDNT_RESOLVE_HOST"
               |  Curl.CURLE_COULDNT_CONNECT             -> "CURLE_COULDNT_CONNECT"
               |  Curl.CURLE_FTP_WEIRD_SERVER_REPLY      -> "CURLE_FTP_WEIRD_SERVER_REPLY"
               |  Curl.CURLE_FTP_ACCESS_DENIED           -> "CURLE_FTP_ACCESS_DENIED"
               |  Curl.CURLE_FTP_USER_PASSWORD_INCORRECT -> "CURLE_FTP_USER_PASSWORD_INCORRECT"
               |  Curl.CURLE_FTP_WEIRD_PASS_REPLY        -> "CURLE_FTP_WEIRD_PASS_REPLY"
               |  Curl.CURLE_FTP_WEIRD_USER_REPLY        -> "CURLE_FTP_WEIRD_USER_REPLY"
               |  Curl.CURLE_FTP_WEIRD_PASV_REPLY        -> "CURLE_FTP_WEIRD_PASV_REPLY"
               |  Curl.CURLE_FTP_WEIRD_227_FORMAT        -> "CURLE_FTP_WEIRD_227_FORMAT"
               |  Curl.CURLE_FTP_CANT_GET_HOST           -> "CURLE_FTP_CANT_GET_HOST"
               |  Curl.CURLE_FTP_CANT_RECONNECT          -> "CURLE_FTP_CANT_RECONNECT"
               |  Curl.CURLE_FTP_COULDNT_SET_BINARY      -> "CURLE_FTP_COULDNT_SET_BINARY"
               |  Curl.CURLE_PARTIAL_FILE                -> "CURLE_PARTIAL_FILE"
               |  Curl.CURLE_FTP_COULDNT_RETR_FILE       -> "CURLE_FTP_COULDNT_RETR_FILE"
               |  Curl.CURLE_FTP_WRITE_ERROR             -> "CURLE_FTP_WRITE_ERROR"
               |  Curl.CURLE_FTP_QUOTE_ERROR             -> "CURLE_FTP_QUOTE_ERROR"
               |  Curl.CURLE_HTTP_NOT_FOUND              -> "CURLE_HTTP_NOT_FOUND"
               |  Curl.CURLE_WRITE_ERROR                 -> "CURLE_WRITE_ERROR"
               |  Curl.CURLE_MALFORMAT_USER              -> "CURLE_MALFORMAT_USER"
               |  Curl.CURLE_FTP_COULDNT_STOR_FILE       -> "CURLE_FTP_COULDNT_STOR_FILE"
               |  Curl.CURLE_READ_ERROR                  -> "CURLE_READ_ERROR"
               |  Curl.CURLE_OUT_OF_MEMORY               -> "CURLE_OUT_OF_MEMORY"
               |  Curl.CURLE_OPERATION_TIMEOUTED         -> "CURLE_OPERATION_TIMEOUTED"
               |  Curl.CURLE_FTP_COULDNT_SET_ASCII       -> "CURLE_FTP_COULDNT_SET_ASCII"
               |  Curl.CURLE_FTP_PORT_FAILED             -> "CURLE_FTP_PORT_FAILED"
               |  Curl.CURLE_FTP_COULDNT_USE_REST        -> "CURLE_FTP_COULDNT_USE_REST"
               |  Curl.CURLE_FTP_COULDNT_GET_SIZE        -> "CURLE_FTP_COULDNT_GET_SIZE"
               |  Curl.CURLE_HTTP_RANGE_ERROR            -> "CURLE_HTTP_RANGE_ERROR"
               |  Curl.CURLE_HTTP_POST_ERROR             -> "CURLE_HTTP_POST_ERROR"
               |  Curl.CURLE_SSL_CONNECT_ERROR           -> "CURLE_SSL_CONNECT_ERROR"
               |  Curl.CURLE_FTP_BAD_DOWNLOAD_RESUME     -> "CURLE_FTP_BAD_DOWNLOAD_RESUME"
               |  Curl.CURLE_FILE_COULDNT_READ_FILE      -> "CURLE_FILE_COULDNT_READ_FILE"
               |  Curl.CURLE_LDAP_CANNOT_BIND            -> "CURLE_LDAP_CANNOT_BIND"
               |  Curl.CURLE_LDAP_SEARCH_FAILED          -> "CURLE_LDAP_SEARCH_FAILED"
               |  Curl.CURLE_LIBRARY_NOT_FOUND           -> "CURLE_LIBRARY_NOT_FOUND"
               |  Curl.CURLE_FUNCTION_NOT_FOUND          -> "CURLE_FUNCTION_NOT_FOUND"
               |  Curl.CURLE_ABORTED_BY_CALLBACK         -> "CURLE_ABORTED_BY_CALLBACK"
               |  Curl.CURLE_BAD_FUNCTION_ARGUMENT       -> "CURLE_BAD_FUNCTION_ARGUMENT"
               |  Curl.CURLE_BAD_CALLING_ORDER           -> "CURLE_BAD_CALLING_ORDER"
               |  Curl.CURLE_HTTP_PORT_FAILED            -> "CURLE_HTTP_PORT_FAILED"
               |  Curl.CURLE_BAD_PASSWORD_ENTERED        -> "CURLE_BAD_PASSWORD_ENTERED"
               |  Curl.CURLE_TOO_MANY_REDIRECTS          -> "CURLE_TOO_MANY_REDIRECTS"
               |  Curl.CURLE_UNKNOWN_TELNET_OPTION       -> "CURLE_UNKNOWN_TELNET_OPTION"
               |  Curl.CURLE_TELNET_OPTION_SYNTAX        -> "CURLE_TELNET_OPTION_SYNTAX"
               |  Curl.CURLE_OBSOLETE                    -> "CURLE_OBSOLETE"
               |  Curl.CURLE_SSL_PEER_CERTIFICATE        -> "CURLE_SSL_PEER_CERTIFICATE"
               |  Curl.CURLE_GOT_NOTHING                 -> "CURLE_GOT_NOTHING"
               |  Curl.CURLE_SSL_ENGINE_NOTFOUND         -> "CURLE_SSL_ENGINE_NOTFOUND"
               |  Curl.CURLE_SSL_ENGINE_SETFAILED        -> "CURLE_SSL_ENGINE_SETFAILED"
               |  Curl.CURLE_SEND_ERROR                  -> "CURLE_SEND_ERROR"
               |  Curl.CURLE_RECV_ERROR                  -> "CURLE_RECV_ERROR"
               |  Curl.CURLE_SHARE_IN_USE                -> "CURLE_SHARE_IN_USE"
               |  Curl.CURLE_SSL_CERTPROBLEM             -> "CURLE_SSL_CERTPROBLEM"
               |  Curl.CURLE_SSL_CIPHER                  -> "CURLE_SSL_CIPHER"
               |  Curl.CURLE_SSL_CACERT                  -> "CURLE_SSL_CACERT"
               |  Curl.CURLE_BAD_CONTENT_ENCODING        -> "CURLE_BAD_CONTENT_ENCODING"
               |  Curl.CURLE_LDAP_INVALID_URL            -> "CURLE_LDAP_INVALID_URL"
               |  Curl.CURLE_FILESIZE_EXCEEDED           -> "CURLE_FILESIZE_EXCEEDED"
               |  Curl.CURLE_FTP_SSL_FAILED              -> "CURLE_FTP_SSL_FAILED"
               |  Curl.CURLE_USE_SSL_FAILED              -> "CURLE_USE_SSL_FAILED"
               |  Curl.CURLE_SEND_FAIL_REWIND            -> "CURLE_SEND_FAIL_REWIND"
               |  Curl.CURLE_SSL_ENGINE_INITFAILED       -> "CURLE_SSL_ENGINE_INITFAILED"
               |  Curl.CURLE_LOGIN_DENIED                -> "CURLE_LOGIN_DENIED"
               |  Curl.CURLE_TFTP_NOTFOUND               -> "CURLE_TFTP_NOTFOUND"
               |  Curl.CURLE_TFTP_PERM                   -> "CURLE_TFTP_PERM"
               |  Curl.CURLE_REMOTE_DISK_FULL            -> "CURLE_REMOTE_DISK_FULL"
               |  Curl.CURLE_TFTP_ILLEGAL                -> "CURLE_TFTP_ILLEGAL"
               |  Curl.CURLE_TFTP_UNKNOWNID              -> "CURLE_TFTP_UNKNOWNID"
               |  Curl.CURLE_REMOTE_FILE_EXISTS          -> "CURLE_REMOTE_FILE_EXISTS"
               |  Curl.CURLE_TFTP_NOSUCHUSER             -> "CURLE_TFTP_NOSUCHUSER"
               |  Curl.CURLE_CONV_FAILED                 -> "CURLE_CONV_FAILED"
               |  Curl.CURLE_CONV_REQD                   -> "CURLE_CONV_REQD"
               |  Curl.CURLE_SSL_CACERT_BADFILE          -> "CURLE_SSL_CACERT_BADFILE"
               |  Curl.CURLE_REMOTE_FILE_NOT_FOUND       -> "CURLE_REMOTE_FILE_NOT_FOUND"
               |  Curl.CURLE_SSH                         -> "CURLE_SSH"
               |  Curl.CURLE_SSL_SHUTDOWN_FAILED         -> "CURLE_SSL_SHUTDOWN_FAILED"
               |  Curl.CURLE_AGAIN                       -> "CURLE_AGAIN"
           in
             Some (Printf.sprintf 
                     "Curl.CurlException (%s, %d, %S)" 
                     curl_str
                     http_code 
                     str)
       | _ ->
           None)

let verbose = ref true

let odb = 
  let rebase fn = 
    FilePath.make_filename ["test"; "data"; "storage"; fn]
  in
    ref 
      (ODBContext.default (rebase "incoming"))

let ocsigen = ref "ocsigen"
let ocsigen_args = ref ["-s"]

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
       <uploaddir>%s</uploaddir>
       <maxuploadfilesize>4MB</maxuploadfilesize>"
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
          InotifyExt.assert_create_file command_fn
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
     "<ocsigen>
        <server>
          $std_conf
          <extension findlib-package=\"oasis\" />
          <extension findlib-package=\"sexplib\" />
          <extension findlib-package=\"inotify\" />
          <extension findlib-package=\"markdown\" />
          <extension findlib-package=\"markdown.html\" />
          <extension findlib-package=\"cameleon.rss\" />
          <extension findlib-package=\"yojson\" />
          <extension findlib-package=\"curl\" />
          <extension findlib-package=\"sqlexpr\" />
          <extension findlib-package=\"ocamlcore-api.ocsigen\" />

          <host charset=\"utf-8\" >
            <site path=\"\">
              <eliom module=\"$curdir/_build/src/rest/rest.cma\" />
              <eliom module=\"$curdir/_build/src/rest/curl/rest-curl.cma\" />
              <eliom module=\"$curdir/_build/src/rest/ocsigen/rest-ocsigen.cma\" />
              <eliom module=\"$curdir/_build/src/lib/oasis-db.cma\" />
              <eliom module=\"$curdir/_build/src/web/oasis-db-ocsigen.cma\">
                <dir rel=\"incoming\">$rootdir/incoming</dir>
                <dir rel=\"dist\">$rootdir/dist</dir>
                <dir rel=\"mkd\">$curdir/src/web/mkd\"</dir>
                <db>$rootdir/db.sql</db>
                <ocamlcore-api>
                  <stub>true</stub>
                  <base-path>stub</base-path>
                </ocamlcore-api>
              </eliom>
              <static dir=\"$curdir/src/web/static\" /> 
            </site>
          </host>
        </server>
      </ocsigen>"

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

