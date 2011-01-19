
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

let _ :: _ | [] = 

  let set_verbose v = 
    odb := 
    {!odb with 
         ODBContext.logger =
           if v then 
             Lwt_log.channel ~close_mode:`Close ~channel:Lwt_io.stderr ()
           else
             Lwt_log.null};

    ocsigen_args := 
    List.filter 
      (function 
         | "-s" | "--silent" -> not v 
         | _ -> true)
      !ocsigen_args;

    verbose := v
  in

  let arg_specs = 
    [
      "--fake-incoming",
      Arg.String (fun s -> fake_incoming := Some s),
      "prog Fake incoming program.";

      "--ocsigen",
      Arg.Set_string ocsigen,
      "prog Ocsigen launch script.";

      "--ocsigen-args",
      Arg.Rest (fun s -> ocsigen_args := !ocsigen_args @ [s]),
      "args* Ocsigen args.";
    ]
  in

    run_test_tt_main
      ~arg_specs
      ~set_verbose
      ("OASIS-DB">:::
       [
         TestCompletion.tests;
         (* TestIncoming.tests; *)
         TestAPI.tests;
         TestSync.tests;
       ])
