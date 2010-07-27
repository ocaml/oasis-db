
open Lwt

(*
let uncompressed_dirs = 
  List.map 
    (fun fn ->
       ODBArchive.uncompress_tmp_dir fn 
       (fun fn an dn ->
         ODBPkgVerCompletion.run fn an dn 
         >>= fun (pkg_lst, ver_lst) ->
         Lwt_io.printl
           (fn^": "^
             (String.concat ", " pkg_lst)^"; "^
             (String.concat ", " ver_lst))))
    [
      (*
      "../test/foo-0.1.0.tar.gz";
      "../test/bar-0.2.0.tar.bz2";
      *)
      "../test/baz_3.O~alpha1.zip";
    ]
    *)

let ctxt = 
  ODBContext.default

let rec tic_tac () =
  let aux str () =
    Lwt_io.printl str
    >>= fun () ->
    Lwt_unix.sleep 1.0
  in
    aux "tic" () >>= aux "tac" >>= tic_tac

let incoming_monitor () =
 ODBInotify.monitor_low ~ctxt ~recurse:false 
   (fun ev () ->
     Lwt_io.printl (ODBInotify.string_of_watch_kind_event ev))
   ODBConf.incoming_dir ()

let () = 
  Lwt_main.run
    (ODBStorage.init ~ctxt ()
     >>= fun () ->
      (*join uncompressed_dirs *)
     ODBStorage.fold_packages
      (fun t acc -> t :: acc) 
      []
     >>= fun lst ->
     Lwt_io.printl ("Packages: "^(String.concat ", " lst))
     >>= fun () ->
     Lwt.choose [tic_tac (); incoming_monitor ()]
     >>= fun () -> 
     Lwt_io.printl "This is the end!")


