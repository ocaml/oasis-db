
open OUnit
open TestCommon
open Lwt

module FS = ODBFilesystem

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
    ignore 

    (* Main *)
    (fun ocs ->
       let ctxt = !odb in

         bracket_tmpdir

           (fun cache_dir ->
              let add_spy fs = 
                if !TestCommon.verbose then
                  FS.spy fs
                else
                  return ()
              in
              (* Create src sync *)
              let src_fs = 
                let dist = 
                  FilePath.concat ocs.ocs_rootdir "dist"
                in
                  FileUtil.mkdir dist;
                  new FS.std_rw dist
              in
              let rsrc_sync =
                ref
                  (Lwt_unix.run 
                     (add_spy src_fs
                      >>= fun () ->
                      ODBSync.create ~ctxt src_fs))
              in
              let () = 
                Lwt_unix.run
                  (ODBSync.autoupdate rsrc_sync
                   >>= fun () ->
                   ODBSync.scan rsrc_sync)
              in

              (* Create tgt sync *)
              let tgt_fs = 
                new FS.std_rw cache_dir
              in

              let tgt_sync = 
                Lwt_unix.run
                  (add_spy tgt_fs
                   >>= fun () ->
                   ODBSync.create ~ctxt tgt_fs
                   >|= fun sync ->
                   new ODBSync.remote sync (ocs.ocs_base_url^"dist"))
              in

              let assert_fn (fn, ctnt) exp_exists =
                let fn_exists = Lwt_unix.run (tgt_sync#file_exists fn)
                in
                let () =
                  assert_equal 
                    ~msg:fn
                    ~printer:string_of_bool
                    exp_exists fn_exists
                in
                  if exp_exists then
                    let ctnt' =
                      Lwt_unix.run
                        (FS.with_file_in tgt_sync fn
                           (LwtExt.IO.with_file_content_chn ~fn))
                    in
                      assert_equal
                        ~msg:(Printf.sprintf "File content of '%s'" fn)
                        ~printer:(fun s -> s)
                        ctnt ctnt'
              in

              let add_fn (fn, cnt) =
                Lwt_unix.run
                  (src_fs#mkdir 
                     ~ignore_exist:true 
                     (FilePath.dirname fn) 
                     0o755
                   >>= fun () ->
                   FS.with_file_out src_fs fn 
                     (fun chn -> 
                        Lwt_io.write chn cnt))
              in

              let update () =
                Lwt_unix.run tgt_sync#update
              in

              let extra_fn = ("extra.fn", "abcde") in
              let () =
                add_fn extra_fn;
                assert_fn extra_fn false;
                update ();
                assert_fn extra_fn true
              in

              let more_fns =
                [
                  "foo/storage.sexp", "zzzzz";
                  "bar/_oasis", "yyyyy";
                ]
              in
              let () = 
                List.iter add_fn more_fns;
                update ();
                List.iter (fun fn -> assert_fn fn true) more_fns
              in
                ()
           )
           ())

    (* Post stop *)
    ignore
