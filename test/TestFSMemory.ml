
(** Tests for ODBFSMemory filesystem
    
    @author Sylvain Le Gall
  *)

open OUnit
open Lwt

let tests =
  "ODBFSMemory" >:::
  [
    "Simple" >::
    (fun () -> 
     let fstree = ODBFSTree.root () in
     let fs = new ODBFSMemory.read_write fstree in
       Lwt_main.run
         (ODBVFS.with_file_out fs "foo"
            (fun chn ->
               Lwt_io.write chn "abcde")
          >>= fun () ->
          fs#mkdir "bar" 0o755
          >>= fun () ->
          ODBVFS.with_file_in fs "foo"
            (fun chn ->
               LwtExt.IO.with_file_content_chn chn)
          >>= fun foo_cntn ->
          fs#is_directory "bar"
          >>= fun bar_dir ->
          try
            assert_equal 
              ~msg:"foo content"
              ~printer:(fun s -> s)
              "abcde"
              foo_cntn;
            assert_bool
              "bar is a directory"
              bar_dir;
            return ()
          with e ->
            fail e));

    "cp_directories" >::
    (fun () ->
       let fs1 = new ODBFSMemory.read_write (ODBFSTree.root ()) in
       let fs2 = new ODBFSMemory.read_write (ODBFSTree.root ()) in
         Lwt_main.run
           (Lwt_list.iter_s
              (fun dn -> fs1#mkdir dn 0o755)
              [
                "foo";
                "foo/bar";
                "foo/baz";
              ]
            >>= fun () ->
            ODBFSMemory.cp_directories fs1 fs2
            >>= fun () ->
            fs2#is_directory "foo/bar"
            >>= fun foo_bar_exists ->
            try 
              assert_bool
                "foo/bar is a directory"
                foo_bar_exists;
              return ()
            with e ->
              fail e));
  ]
