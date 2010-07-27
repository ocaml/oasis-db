
open Lwt
open ODBGettext

exception EStatus of string * string list * Unix.process_status;;

let string_of_exception = 
  function
    | EStatus (cmd, args, st) ->
        begin
          let cmd_str = 
            String.concat " " (cmd :: args)
          in
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
      ?section 
      ?logger 
      ?timeout 
      ?env 
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
                  Lwt_log.error ?section ?logger 
                    (string_of_exception exc)
                  >>= fun () ->
                  fail exc
              end
          end

      | StatusFunction f ->
          begin
            f st
          end
  in

  let p = 
    Lwt_process.open_process_full
      ?timeout ?env (cmd, Array.of_list (cmd :: args))
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
    Lwt_log.debug_f ?section ?logger
      (f_ "Running '%s'")
      (String.concat " " (cmd :: args))
    >>= fun () ->
    Lwt_io.close p#stdin
    >>= fun () ->
      join 
        [       
          read_and_log (Lwt_log.info ?section ?logger) p#stdout;
          read_and_log (Lwt_log.error ?section ?logger) p#stderr;
        ]
    >>= fun () ->
    p#close 
    >>= 
    handle_status 

