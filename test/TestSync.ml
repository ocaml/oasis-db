
open OUnit
open TestCommon
open Lwt

let tests = 
  "Sync" >::
  bracket_ocsigen 
    ("<ocsigen>
        <server>
          $std_conf
          <host>
            <site path=\"\">
              <static dir=\"$rootdir\" />
            </site>
          </host>
        </server>
      </ocsigen>")

    (* Pre start *)
    (fun ocs ->
       let ctxt = !odb in

       let job =
         (* Load synchonization data *)
         ODBSync.load ~ctxt ocs.ocs_rootdir
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

              let extra_fn = "extra.fn" in

              let update_job =
                let abs_fn = 
                  FilePath.concat ocs.ocs_rootdir extra_fn
                in
                  FileUtil.touch abs_fn;
                  ODBSync.load ~ctxt ocs.ocs_rootdir
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
