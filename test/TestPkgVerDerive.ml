
(** Tests for PkgVerDerive
  * @authors Sylvain Le Gall
  *)

open OUnit
open TestCommon
open OASISVersion

let tests =
  "PkgVerDerive" >::
  (fun () ->
     let mk_ver lst ver = 
       string_of_version
         (ODBDerive.version 
            (List.map version_of_string lst)
            (version_of_string ver))
     in
       assert_equal 
         ~printer:(fun s -> s)
         "0.0.1oasis1"
         (mk_ver ["0.0.1"] "0.0.1");
       assert_equal 
         ~printer:(fun s -> s)
         "0.0.1oasis2"
         (mk_ver ["0.0.1"; "0.0.1oasis1"] "0.0.1oasis1");
       assert_equal 
         ~printer:(fun s -> s)
         "0.0.1oasis0.1"
         (mk_ver ["0.0.1"; "0.0.1oasis1"] "0.0.1");
       assert_equal 
         ~printer:(fun s -> s)
         "0.0.1oasis1"
         (mk_ver ["0.0.1"; "0.0.2"] "0.0.1");
       assert_equal
         ~printer:(fun s -> s)
         "0.0.1oasis1.1"
         (mk_ver ["0.0.1oasis1"; "0.0.1oasis2"] "0.0.1oasis1");
       assert_equal
         ~printer:(fun s -> s)
         "0.0.1oasis1.2"
         (mk_ver ["0.0.1oasis1.1"; "0.0.1oasis2"] "0.0.1oasis1.1");
  )


