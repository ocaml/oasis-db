
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
    ctxt          = ODBContext.default;
  }

let _ = 
  run_test_tt_main
  ("OASIS-DB">:::
    [TestIncoming.tests ctxt])
  
