
open Inotify

let () =
  let fn, prog, args =
    match Array.to_list Sys.argv with 
      | _ :: fn :: prog :: args ->
          fn, prog, args
      | _ ->
          failwith 
            "Bad command line, should be 'inrestart fn prog args*'"
  in

  let fn_bak =
    fn^".bak"
  in

  let fd = 
    Inotify.init ()
  in

  let dn =
    Filename.dirname fn
  in

  let bn =
    Filename.basename fn
  in

  let watch_dn () = 
    if Sys.file_exists dn && Sys.is_directory dn then
      let _wd : wd = 
        add_watch fd dn [S_Close_write; S_Delete_self]
      in
        ()
    else
      failwith 
        (Printf.sprintf "No directory '%s'" dn)
  in

  let () = 
    watch_dn ()
  in

  let cmdline = 
    String.concat " " (prog :: args)
  in

  let run () = 
    Printf.printf "Copy '%s' to '%s'\n%!" fn fn_bak;
    FileUtil.cp [fn] fn_bak;
    Unix.chmod fn_bak 0o700;
    Printf.printf "Running '%s'.\n%!" cmdline;
    Unix.create_process 
      prog
      (Array.of_list (prog :: args)) 
      Unix.stdin
      Unix.stdout
      Unix.stderr
  in

  let pid =
    ref (run ())
  in

  let restart () = 
    begin
      try 
        Unix.kill !pid Sys.sigterm
      with _ ->
        ()
    end;
    begin
      match snd (Unix.waitpid [] !pid) with
        | Unix.WEXITED 0 -> 
            ()
        | Unix.WEXITED n | Unix.WSIGNALED n | Unix.WSTOPPED n ->
            Printf.printf 
              "Command line '%s' exited with code %d\n%!"
              cmdline
              n
    end;
    pid := run ()
  in

    while true do 
      ignore (Unix.select [fd] [] [] (-1.));
      List.iter
        (fun (wd, evs, i32, fn_opt) ->
           if List.mem Close_write evs then
             begin
               match fn_opt with 
                 | Some fn -> 
                     begin
                       if fn = bn then
                         restart ()
                     end
                 | None -> ()
             end;

           if List.mem Delete_self evs then
             begin
               (*rm_watch fd wd;*)
               Printf.printf
                 "Directory '%s' has been removed, waiting for it to come back.\n%!"
                 dn;
               while not (Sys.file_exists dn) do
                 ignore (Unix.select [] [] [] 0.1);
               done;
               Printf.printf
                 "Directory '%s' is back online.\n%!"
                 dn;
               if Sys.file_exists fn then
                 restart ();
               watch_dn ();
             end;

           ())
        (Inotify.read fd)
    done;
    (* Never reached... but this is what we should do *)
    Unix.close fd

