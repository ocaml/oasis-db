
open OUnit
open TestCommon
open Lwt

let tests = 
  "Sync" >::
  bracket_ocsigen 
    (Printf.sprintf
       "<ocsigen>
          <server>
            $std_conf
            <host>
              <site path=\"\">
                <static dir=\"$curdir/%s\" />
              </site>
            </host>
          </server>
        </ocsigen>"
       !odb.ODBContext.dist_dir)

    (* Pre start *)
    (fun _ ->
       let ctxt = !odb in
       (* Put in place the synchronization data *)

       let add_exists pkg_str ver_str sync fn =
         ODBStorage.PkgVer.filename pkg_str ver_str fn 
         >>= fun fn ->
           if Sys.file_exists fn then
             ODBSync.add fn sync
           else
             return sync
       in

       let job =
         (* Load synchonization data *)
         ODBFileUtil.rm 
           ~ctxt
           (List.map 
              (FilePath.concat ctxt.ODBContext.dist_dir) 
              ["sync-meta.sexp"; "sync.sexp"])
         >>= fun () ->
         ODBSync.load ~ctxt ctxt.ODBContext.dist_dir
         >>= fun sync ->

         (* Scan all the storage and add files to sync *)
         ODBStorage.init ~ctxt () 
         >>= fun () ->
         ODBStorage.Pkg.elements () 
         >>=
         Lwt_list.fold_left_s
           (fun sync pkg_str ->
              ODBStorage.Pkg.filename pkg_str (`PluginData "storage")
              >>= fun fn ->
              ODBSync.add fn sync
              >>= fun sync ->
              
              (* Iterate in package version *)
              ODBStorage.PkgVer.elements pkg_str 
              >>= 
              Lwt_list.fold_left_s
                (fun sync ver ->
                   Lwt_list.fold_left_s 
                     (add_exists pkg_str 
                        (OASISVersion.string_of_version ver.ODBPkgVer.ver))
                     sync
                     [`OASIS; 
                      `OASISPristine; 
                      `Tarball; 
                      `PluginData "storage"])
                sync)
           sync
         >>= fun sync ->

         (* Dump synchronization data *)
         ODBSync.dump ~ctxt sync
       in

         Lwt_unix.run job)

    (* Main *)
    (fun ocs ->
       let ctxt = !odb in

         bracket
           (fun () ->
              let dn =
                Filename.temp_file "oasis-db-" ".dir"
              in
                FileUtil.rm [dn];
                FileUtil.mkdir dn;
                dn)

           (fun cache_dir ->
              let job = 
                ODBSync.Remote.create ~ctxt cache_dir ocs.ocs_base_url
                >>= fun rmt ->
                ODBSync.Remote.update ~ctxt rmt
                >>= fun () ->
                ODBSync.Remote.update ~ctxt rmt
              in
              let () = 
                Lwt_unix.run job
              in
              let assert_fn mkfn b =
                let fn = String.concat "/" mkfn in
                  assert_equal 
                    ~msg:fn
                    ~printer:string_of_bool
                    b
                    (Sys.file_exists 
                       (FilePath.concat 
                          cache_dir
                          (FilePath.make_filename mkfn)))
              in
              let () = 
                assert_fn ["oasis"; "storage.sexp"] true;
                assert_fn ["ounit"; "1.1.0"; "ounit-1.1.0.tar.gz"] true
              in

              let extra_fn = 
                "test.update"
              in
              let update_job =
                let abs_fn = 
                  FilePath.concat ctxt.ODBContext.dist_dir extra_fn
                in
                  FileUtil.touch abs_fn;
                  ODBSync.load ~ctxt ctxt.ODBContext.dist_dir
                  >>=
                  ODBSync.add abs_fn
                  >>=
                  ODBSync.dump ~ctxt
                  >>= fun () -> 
                  ODBSync.Remote.create ~ctxt cache_dir ocs.ocs_base_url
                  >>= 
                  ODBSync.Remote.update ~ctxt 
              in
                Lwt_unix.run update_job;
                assert_fn [extra_fn] true
           )

           (fun cache_dir ->
              FileUtil.rm ~recurse:true [cache_dir])

         ())

    (* Post stop *)
    (fun _ -> ())
