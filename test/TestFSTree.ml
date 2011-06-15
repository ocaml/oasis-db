
(** Tests for ODBFSTree
    @author Sylvain Le Gall
  *)

open ODBFSTree
open OUnit

let tests = 
  "ODBFSTree" >:::
  [
    "Simple" >::
    (fun () ->
       let t = root () in
         add_dir t "foo" ();
         add_file t "foo/bar.txt" "This is my text";
         assert_equal
           ~msg:"'' elements"
           ~printer:(String.concat "; ")
           ["foo"]
           (elements t "");
         assert_equal
           ~msg:"'foo' elements"
           ~printer:(String.concat "; ")
           ["bar.txt"]
           (elements t "foo");
         assert_bool
           "'foo' exists"
           (mem t "foo");
         assert_bool
           "'foo' is a dir"
           (is_dir t "foo");
         assert_bool
           "'foo/bar.txt' is not a dir"
           (not (is_dir t "foo/bar.txt"));
         assert_equal
           ~msg:"'foo/bar.txt' content"
           ~printer:(fun s -> s)
           "This is my text"
           (find_file t "foo/bar.txt");
         remove t "foo/bar.txt";
         assert_bool
           "'foo/bar.txt' has been removed"
           (not (mem t "foo/bar.txt"));
         ())
  ]
