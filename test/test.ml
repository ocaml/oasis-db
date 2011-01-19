
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
open Lwt


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
       ])


(*

     TestSync.tests ctxt;
let () = 
  let ctxt =
    ctxt.odb
  in

  let add_exists pkg_str ver_str sync fn =
    ODBStorage.Ver.filename pkg_str ver_str fn 
    >>= fun fn ->
      if Sys.file_exists fn then
        ODBSync.add fn sync
      else
        return sync
  in

  let job =
    (* Load synchonization data *)
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
         ODBStorage.Ver.elements pkg_str 
         >>= 
         Lwt_list.fold_left_s
           (fun sync ver ->
              Lwt_list.fold_left_s 
                (add_exists pkg_str 
                   (OASISVersion.string_of_version ver.ODBVer.ver))
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

    Lwt_unix.run job
 *)
