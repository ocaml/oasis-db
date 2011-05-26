
(** Test some web pages loading
   
    @author Sylvain Le Gall
  *)

open OUnit
open TestCommon

let tests =
  "Web" >::
  (TestCLI.bracket_oasis_db_cli 
     (fun ocs oasis_cli ->
        let curl = 
          Curl.init ()
        in
        let write_fun str = 
          String.length str
        in
          assert_command oasis_cli
            ["update"];
          assert_command oasis_cli 
            ["upload";
             in_data_dir "batteries-2.0beta.tar.gz"];
          Curl.set_verbose curl !verbose; 
          Curl.set_followlocation curl true;
          Curl.set_failonerror curl true;
          Curl.set_timeoutms curl 1000;
          Curl.set_writefunction curl write_fun;
          Curl.set_url curl (ocs.ocs_base_url^"view/batteries/2.0beta");
          Curl.perform curl;
          Curl.cleanup curl))

