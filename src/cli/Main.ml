
open BaseMessage
open ODBCLIPlugins

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
    error "%s" (Printexc.to_string e);
    exit 1
