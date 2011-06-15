
(** Curl support functions
  
    @author Sylvain Le Gall
  *)

open ODBGettext
open ExtLib

(** [uri_concat uri path] Concatenates a relative [path] onto an absolute
  * [uri] 
  *)
let uri_concat uri tl = 
  (* TODO: cover more case  of URL concat *)
  if String.ends_with uri "/" then
    uri^tl
  else
    uri^"/"^tl

(** Take care of creating curl socket and closing it
  *)
let with_curl f = 
  (* Generic init of curl *)
  let c = 
    Curl.init () 
  in
  let () = 
    Curl.set_failonerror c true
  in
  let cleanup () = 
    Curl.cleanup c
  in
    try 
      f c;
      cleanup ()
    with e ->
      cleanup ();
      raise e

let download_chn' ?curl ?(custom=ignore) url fn chn = 
  let curl_write fn chn d = 
    output_string chn d;
    String.length d
  in
  let do_download c =
    try 
      (* Resume download *)
      Curl.set_url c url;
      Curl.set_writefunction c (curl_write fn chn);
      custom c;
      Curl.perform c
    with 
      | Curl.CurlException(Curl.CURLE_HTTP_NOT_FOUND, _, _) ->
          failwith 
            (Printf.sprintf
               (f_ "URL '%s' not found to download file '%s'")
               url fn)
      | e ->
          raise e
  in
    match curl with 
      | Some c ->
          do_download c
      | None ->
          with_curl do_download

let download_fn ?curl ?custom url fn = 
  let chn = 
    open_out fn 
  in
  let cleanup () = 
    close_out chn
  in
    try 
      download_chn' ?curl ?custom url fn chn;
      cleanup ()
    with e ->
      cleanup ();
      raise e

module Lwt =
struct 

  open Lwt

  let download_chn ?curl ?custom url fn chn =
    let fn', chn' = 
      Filename.open_temp_file "oasis-db-curl" ""
    in
      finalize
        (fun () ->
           try 
             let () = 
               download_chn' ?curl ?custom url fn chn';
               close_out chn'
             in
               Lwt_io.with_file ~mode:Lwt_io.input fn'
                 (fun chn' ->
                    Lwt_io.write_chars chn 
                      (Lwt_io.read_chars chn'))
           with e ->
             fail e)
        (fun () ->
           (try close_out chn' with e -> ());
           Sys.remove fn';
           return ())

end

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

