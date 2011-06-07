
open Lwt
open ODBGettext
open ODBMessage

exception EStatus of string * string list * Unix.process_status;;

let () = 
  Printexc.register_printer
    (function
       | EStatus (cmd, args, st) ->
           begin
             let cmd_str = 
               String.concat " " (cmd :: args)
             in
             let msg =
               match st with 
                 | Unix.WEXITED n ->
                     Printf.sprintf 
                       (f_ "Process '%s' exited with error code %d")
                       cmd_str
                       n
                     
                 | Unix.WSIGNALED n ->
                     Printf.sprintf
                       (f_ "Process '%s' killed by signal %d")
                       cmd_str
                       n

              | Unix.WSTOPPED n ->
                  Printf.sprintf
                    (f_ "Process '%s' stopped by signal %d")
                    cmd_str
                    n
        end
    |  e  ->
        raise e 

type handle_status = 
  | StatusIgnore
  | StatusExpected of Unix.process_status list
  | StatusFunction of (Unix.process_status -> unit Lwt.t)

let run_logged 
      ~ctxt ?timeout ?env 
      ?stdin_fd
      ?(status=StatusExpected [Unix.WEXITED 0])
      cmd args = 

  let handle_status st =
    (* Handle status *)
    match status with 
      | StatusIgnore ->
          begin
            return ()
          end

      | StatusExpected lst ->
          begin
            if List.mem st lst then 
              begin
                return ()
              end
            else
              begin
                let exc = 
                  EStatus (cmd, args, st)
                in
                  error ~ctxt "%s" (string_of_exception exc)
                  >>= fun () ->
                  fail exc
              end
          end

      | StatusFunction f ->
          begin
            f st
          end
  in

  let read_and_log f chn = 
    let rec read_and_log_aux () =
      catch
        (fun () -> 
           Lwt_io.read_line chn
           >>= f
           >>= read_and_log_aux)
        (function
           | End_of_file ->
               return ()
           | e ->
               fail e)
    in
      read_and_log_aux ()
  in
    begin
      let cmd_line = 
        String.concat " " (cmd :: args)
      in
        match stdin_fd with
          | Some fd ->
              debug ~ctxt (f_ "Running '%s' with fd as input") 
                cmd_line
          | None ->
              debug ~ctxt (f_ "Running '%s'") 
                cmd_line
    end
    >>= fun () ->
    Lwt_process.with_process_full
      ?timeout ?env 
      (cmd, Array.of_list (cmd :: args))
      (fun p ->
         let stdin_task =
           match stdin_fd with 
             | Some fd ->
                 (* Copy fd into stdin of the process *)
                 let chn = 
                   Lwt_io.of_unix_fd 
                     ~mode:Lwt_io.input 
                     (Unix.dup fd)
                 in
                   finalize
                     (fun () ->
                        Lwt_io.write_chars
                          p#stdin
                          (Lwt_io.read_chars chn))
                     (fun () ->
                        Lwt_io.close chn
                        >>= fun () ->
                        Lwt_io.close p#stdin)

             | None ->
                 Lwt_io.close p#stdin
         in
           join 
             [       
               read_and_log (info ~ctxt "%s") p#stdout;
               read_and_log (error ~ctxt "%s") p#stderr;
               stdin_task;
             ]
         >>= fun () ->
         p#close 
         >>=  
         handle_status)

