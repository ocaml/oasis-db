
open TestCommon
open TestCLI
open OUnit

let () = 
  Findlib.init ()

let tests = 
  "Bundle" >::
  bracket_oasis_db_cli
    (fun ocs oasis_cli ->
       bracket_tmpdir 
         (fun tmpdir -> 
            let metas_dir = 
              Filename.concat tmpdir "METAS"
            in
            let site_lib_dir = 
              Filename.concat tmpdir "site-lib"
            in
            let findlib_conf_fn =
              Filename.concat tmpdir "findlib.conf"
            in
            let env_export nm vl env = 
              let replaced = ref false in
                for i = 0 to Array.length env - 1 do 
                  if OASISString.starts_with ~what:(nm^"=") env.(i) then
                    begin
                      env.(i) <- nm^"="^vl;
                      replaced := true
                    end
                done;
                if not !replaced then
                  Array.append env [|nm^"="^vl|]
                else 
                  env
            in
            let bundle_fn = 
              Filename.concat tmpdir "oasis-bunlde-0.3.0~rc5.tar.gz"
            in
              assert_command oasis_cli ["update"];
              List.iter 
                (fun fn ->
                   assert_command oasis_cli ["-debug"; "upload"; in_data_dir fn])
                ["ocamlmod-0.0.3.tar.gz"; "ocamlify-0.0.1.tar.gz"; 
                 "type-conv-3.0.4.tar.gz"; "ocaml-data-notation-0.0.8.tar.gz"];
              assert_command oasis_cli ["update"];
              (* TODO: remove *)
              assert_command oasis_cli
                ["bundle"; in_data_dir "oasis-0.3.0~rc5.tar.gz"];
              assert_command oasis_cli
                ["bundle"; in_data_dir "oasis-0.3.0~rc5.tar.gz"; "-output"; bundle_fn];
              assert_command "tar" ["-C"; tmpdir; "-xzf"; bundle_fn];
              FileUtil.mkdir metas_dir;
              begin
                let sys_metas_dir = Findlib.meta_directory () in
                  List.iter 
                    (fun pkg ->
                       try 
                         let org_fn =
                           List.find
                             (fun fn -> Sys.file_exists fn)
                             ((Filename.concat sys_metas_dir ("META"^"."^pkg)) ::
                              List.map 
                                (fun dn -> FilePath.make_filename [dn; pkg; "META"])
                                (Findlib.search_path ()))
                         in
                         let dst_fn = Filename.concat metas_dir ("META."^pkg) in
                           FileUtil.cp [org_fn] dst_fn
                       with Not_found ->
                         failwith 
                           (Printf.sprintf "Cannot find META for package '%s'" pkg))
                     (* TODO: why threads is needed *)
                     ["unix"; "dynlink"; "camlp4"; "str"; "threads"]
              end;
              FileUtil.mkdir site_lib_dir;
              FileUtil.touch findlib_conf_fn;
              begin
                let ctxt = ODBContext.to_oasis !odb in  
                let findlib_dir = 
                  Findlib.package_directory "findlib"
                in
                  OASISFileUtil.cp ~ctxt ~recurse:true findlib_dir site_lib_dir
              end;
              bracket
                (fun () -> Sys.getcwd ())
                (fun _ ->
                   Sys.chdir (Filename.concat tmpdir "oasis-bundle-0.3.0~rc5");
                   assert_command 
                     ~env:(env_export 
                             "OCAMLPATH" (metas_dir^":"^site_lib_dir)
                             (env_export
                                "OCAMLFIND_CONF" findlib_conf_fn
                                (Unix.environment ())))
                      "ocaml" ["bundle.ml"]) 
                (fun dn -> Sys.chdir dn)
                ())
         ())
