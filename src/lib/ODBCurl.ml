
(** Curl support functions
  
    @author Sylvain Le Gall
  *)

open ODBGettext

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

let download_chn ?curl ?(custom=ignore) url fn chn = 
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
      download_chn ?curl ?custom url fn chn;
      cleanup ()
    with e ->
      cleanup ();
      raise e
