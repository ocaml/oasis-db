
(** 'upload' subcommand handler 
   
     Upload a tarball which contains a _oasis file.

     @author Sylvain Le Gall
  *)


open SubCommand
open OASISUtils
open OASISTypes
open ODBGettext
open ODBCLICommon
open ODBRepository
open Lwt
open ExtLib

let tarball_fn = ref []

let repo = ref None

let publink = ref None

let main () = 
  let ctxt = 
    Lwt_unix.run (context_lwt ())
  in
  let tarballs = 
    match !tarball_fn, !publink with
      | [fn], publink ->
          [fn, publink]
      | [], _ ->
          failwith 
            (s_ "No tarball to upload")
      | lst, Some _ ->
          failwith 
            (s_ "Cannot upload several tarballs with a single publink")
      | lst, None ->
          List.map (fun e -> e, None) lst
  in
  let api_uri = 
    let repo, _ =
      match !repo with 
        | Some nm ->
            begin
              try 
                List.find 
                  (fun (repo, _) -> repo.repo_name = nm)
                  ctxt.cli_repos
              with Not_found ->
                failwith 
                  (Printf.sprintf 
                     (f_ "Unable to find repository named '%s'")
                     nm)
            end
        | None ->
            begin
              try 
                List.find
                  (fun (repo, _) -> repo.repo_api_uri <> None)
                  ctxt.cli_repos
              with Not_found ->
                failwith
                  (s_ "Unable to find a repository where upload is allowed")
            end

    in
      match repo.repo_api_uri with
        | Some uri -> uri
        | None ->
            failwith 
              (Printf.sprintf 
                 (f_ "Selected repository '%s' doesn't have an API URI set")
                 repo.repo_name)
  in
  let curl = 
    Curl.init ()
  in

  let msg_split str =
    List.map String.strip (String.nsplit (String.strip str) "\n")
  in

  let curl_debug _ dbg_typ str =
    let hdr, display =
      match dbg_typ with 
        | Curl.DEBUGTYPE_TEXT -> "text", true
        | Curl.DEBUGTYPE_HEADER_IN -> "header-in", true
        | Curl.DEBUGTYPE_HEADER_OUT -> "header-out", true
        | Curl.DEBUGTYPE_DATA_IN -> "data-in", true
        | Curl.DEBUGTYPE_DATA_OUT -> "data-out", false
        | Curl.DEBUGTYPE_END -> "end", true
    in
      if display then 
        List.iter 
          (fun s -> BaseMessage.debug "curl %s: %s" hdr s)
          (msg_split str)
  in

  let answer = Buffer.create 13 
  in
  let curl_write d =
    Buffer.add_string answer d;
    String.length d
  in
    Curl.set_followlocation curl true;
(*     Curl.set_failonerror curl true; *)
    Curl.set_verbose curl true;
    Curl.set_debugfunction curl curl_debug;
    Curl.set_httpheader curl ["Accept: text/plain"]; 
    Curl.set_writefunction curl curl_write;

    (* Login *)
    Curl.set_url curl 
      (* TODO: login/password *)
      (ODBCurl.uri_concat api_uri "login?login=admin1&password=");
    Curl.set_cookiefile curl ""; (* Enabled in-memory cookie *)
    Curl.perform curl;

    (* Uploads *)
    List.iter 
      (fun (fn, publink) ->
         let publink = Some "foo" in
         let post_params = 
            [Curl.CURLFORM_FILE("tarball", fn, Curl.DEFAULT)]
         in
         let post_params = 
           match publink with 
             | Some uri ->
                 Curl.CURLFORM_CONTENT("publink", uri, Curl.DEFAULT)
                 :: post_params
             | None ->
                 post_params
         in
         let () =
           Curl.set_url curl (ODBCurl.uri_concat api_uri "upload");
           Curl.set_post curl true;
           Curl.set_httppost curl post_params;
           Curl.perform curl
         in
         let http_code = 
           Curl.get_httpcode curl
         in
         let msg_code = 
           Printf.sprintf (f_ "HTTP code %d") http_code
         in
         let msg_lst = 
           msg_split (Buffer.contents answer)
         in
           if 200 <= http_code && http_code < 400 then
             begin
               BaseMessage.debug "%s" msg_code;
               List.iter 
                 (fun s -> 
                    BaseMessage.info "%s" s)
                 msg_lst
             end
           else
             begin
               List.iter 
                 (fun s ->
                    BaseMessage.error "%s" s)
                 (msg_code :: msg_lst);
               failwith 
                 (Printf.sprintf
                    (f_ "Error while uploading '%s'")
                    fn)
             end;
           Buffer.clear answer)
      tarballs;

    (* Logout *)
    Curl.set_url curl (ODBCurl.uri_concat api_uri "logout");
    Curl.set_post curl false;
    Curl.perform curl;

    Curl.cleanup curl

let scmd = 
  {(SubCommand.make
      "upload"
      (s_ "Upload a tarball to the server")
      ODBCLIData.upload_mkd
      main)
     with 
         scmd_specs =
           [
             "-repo",
             Arg.String (fun s -> repo := Some s),
             "str Define the repository to upload to.";

             "-publink",
             Arg.String (fun s -> publink := Some s),
             "uri Define the public URI from where the tarball \
                  can be downloaded.";
           ];
         scmd_anon =
           (fun fn ->
              tarball_fn := fn :: !tarball_fn);}

let () = 
  SubCommand.register scmd

   
