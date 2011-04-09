
open BaseMessage
open ODBPlugins

let () =
  (* Run subcommand *)
  try 
    let main =
      ArgExt.parse ()
    in
      main ()
  with e ->
    if Printexc.backtrace_status () then
      Printexc.print_backtrace stderr;
    error "%s" (string_of_exception e);
    exit 1
