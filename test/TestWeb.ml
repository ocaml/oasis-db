
(** Test some web pages loading
   
    @author Sylvain Le Gall
  *)

open OUnit
open TestCommon

let test_of_vector (tarballs, urls) = 
  (String.concat "+" urls) >::
  TestCLI.bracket_oasis_db_cli 
    (fun ocs oasis_cli ->
       ODBCurl.with_curl
         (fun curl ->
            let write_fun str = 
              if !verbose then
                prerr_string str;
              String.length str
            in
              assert_command oasis_cli
                ["update"];
              List.iter
                (fun tarball ->
                   assert_command oasis_cli 
                     ["upload";
                      in_data_dir tarball])
                tarballs;
              Curl.set_verbose curl !verbose; 
              Curl.set_followlocation curl true;
              Curl.set_failonerror curl true;
              Curl.set_timeoutms curl 1000;
              Curl.set_writefunction curl write_fun;
              List.iter 
                (fun url ->
                   Curl.set_url curl (ocs.ocs_base_url^url);
                   Curl.perform curl)
                urls))

let tests =
  "Web" >:::
  (List.map 
     test_of_vector
     [
       ["batteries-2.0beta.tar.gz"],
       ["view/batteries/2.0beta"];

       ["oasis-0.2.0.tar.gz";
        "bin_prot-1.3.1.tar.gz"],
       ["dist/"];
     ])
