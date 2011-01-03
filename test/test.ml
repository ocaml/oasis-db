
IFDEF HAS_GETTEXT THEN
module Gettext =
  Gettext.Program
    (struct
       let textdomain   = "oasis"
       let codeset      = None
       let dir          = None
       let dependencies = Gettext.init @ OASISGettext.init
     end)
    (GettextStub.Native)
ELSE
module Gettext =
struct 
  let init = [], ""
end
ENDIF

open OUnit
open TestCommon

let ctxt = 
  {
    fake_incoming = "_build/test/FakeIncoming";
    odb           = ODBContext.default 
                      (FilePath.make_filename ["test"; "data"; "storage"]);
  }

(*
let _ = 
  run_test_tt_main
  ("OASIS-DB">:::
    [TestIncoming.tests ctxt])
  
 *)

(*
let list () = 
  create API.

let () = 
 *)

let base_url = "http://localhost:8080/api/" 
let ctxt = ctxt.odb 

let () = 
  let lst = 
    ODBREST.Pkg.list ~ctxt base_url ()
  in
  let lst' = 
    List.map 
      (fun pkg ->
         pkg,
         ODBREST.PkgVer.latest ~ctxt base_url pkg)
      lst
  in
    List.iter
      (fun (pkg, ver) -> 
         Printf.printf "%s v%s\n" 
           pkg 
           (OASISVersion.string_of_version ver))
      lst'
