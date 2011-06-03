
(** Tests for CLI
     @author Sylvain Le Gall
  *)

open FileUtil
open TestCommon
open OUnit

let bracket_oasis_db_cli f = 
  bracket_oasis_db
    (* Pre start *)
    (fun _ ->
       skip_long ())

    (* Main *)
    (fun ocs ->
       bracket_tmpdir 
         (fun tmpdir ->
            (* Override XDG environment *)
            let oasis_config, oasis_data, oasis_cache = 
              let mkdir_override nm vl =
                let vl = 
                  Filename.concat tmpdir vl
                in
                  mkdir ~parent:true vl;
                  Unix.putenv nm vl;
                  Filename.concat vl "oasis"
              in
                mkdir_override "XDG_CONFIG_HOME" "config",
                mkdir_override "XDG_DATA_HOME"   "local",
                mkdir_override "XDG_CACHE_HOME"  "cache"
            in

            (* Create a temporary config file *)
            let () = 
              let () = mkdir oasis_config in
              let fn  = Filename.concat oasis_config "oasis.ini" in
              let chn = open_out  fn in
              let str =
                Printf.sprintf
                  "[repo]\n\
                   \n\
                   descrs = %sdescr.sexp\n"
                  ocs.ocs_base_url
              in
                if !verbose then
                  begin
                    Printf.eprintf "%s file content:\n%s\n%!"
                      fn str
                  end;
                output_string chn str;
                close_out chn
            in

            let oasis_cli =
              !oasis_cli
            in
              f ocs oasis_cli)
         ())

    (* Post stop *)
    ignore


(* Check that there is no "E: ..." message in the output
 *)
let check_no_error = 
  let rex = Pcre.regexp ~flags:[`MULTILINE] "^E:" in
    fun str ->
      assert_bool
        ("No error in output: \n"^str)
        (not (Pcre.pmatch ~rex str))

(* Dump a char stream and run tests on the output. Useful to check 
 * output of assert_command
 *)
let check_stream lst strm = 
  let str = 
    let buf = Buffer.create 13 in
      Stream.iter (Buffer.add_char buf) strm;
      Buffer.contents buf
  in
    List.iter (fun f -> f str) lst

let foutput_defaults =
  [check_no_error]

(* Default checks *)
let foutput =
  check_stream foutput_defaults


let tests =
  "CLI" >:::
  [
    "Fetch" >::
    bracket_oasis_db_cli
      (fun ocs oasis_cli ->

         let () =
           (* Run an update *)
           assert_command oasis_cli 
  (* TODO: reactivate                ~foutput *)
             ["update"];

           (* Should not find fastrandom *)
           assert_command oasis_cli 
             ~exit_code:(Unix.WEXITED 1)
             ["fetch"; "ocaml-fastrandom"];

           (* Upload fastrandom *)
           assert_command oasis_cli
             ~foutput
             ["upload";
              in_data_dir "ocaml-fastrandom-0.0.1.tar.gz"]; 

           (* Upload ocamlify *)
           assert_command oasis_cli 
             ~foutput
             ["upload";
              in_data_dir "ocamlify-0.0.1.tar.gz"];

           (* Now we can find fastrandom *)
           assert_command oasis_cli 
             ~foutput
             ["update"]
         in
           
         let check_fastrandom = 
           check_stream 
             ((fun str ->
                 assert_bool
                   ("Name is set in output: \n"^str)
                   (Pcre.pmatch ~pat:"Name *: *ocaml-fastrandom" str))
             :: foutput_defaults)
         in
           assert_command oasis_cli 
             ~foutput:check_fastrandom
             ["fetch"; 
              "-what"; "oasis"; 
              "-output"; "content";
              "ocaml-fastrandom"; ">= 0.0.1"];
           assert_command oasis_cli
             ~foutput:check_fastrandom
             ["fetch";
              "-match"; "findlib";
              "-what"; "oasis";
              "-output"; "content";
              "fastrandom"];
           assert_command oasis_cli
             ~foutput
             ["fetch";
              "-match"; "executable";
              "-what"; "oasis";
              "-output"; "content";
              "ocamlify"];
           assert_command oasis_cli
             ~foutput:
             (check_stream 
                [fun str ->
                   let str =
                     ExtLib.String.strip str
                   in
                     assert_bool
                       (Printf.sprintf "File '%s' exists" str)
                       (Sys.file_exists str)])
             ["-quiet"; "fetch";
              "-what"; "tarball";
              "-output"; "filename";
              "ocaml-fastrandom"];
          );

    "Bundle" >::
    bracket_oasis_db_cli
      (fun ocs oasis_cli ->
         let () =
           (* Run an update *)
           assert_command oasis_cli 
  (* TODO: reactivate                ~foutput *)
             ["update"];

           (* Uploads *)
           List.iter 
             (fun fn ->
                assert_command oasis_cli 
                  ~foutput 
                  ["upload"; in_data_dir fn])
             ["ocamlify-0.0.1.tar.gz";
              "ocaml-data-notation-0.0.1.tar.gz";
              "ocaml-expect-0.0.2.tar.gz";
             ];

           (* Run an update *)
           assert_command oasis_cli 
             ~foutput
             ["update"];
         in
           assert_command oasis_cli
             ~foutput
             ["-debug"; "bundle"; 
              in_data_dir "oasis-0.2.0.tar.gz"];

           assert_bool
             "Bundle tarball created"
             (Sys.file_exists "oasis-bundle-0.2.0.tar.gz");
      );
  ]

