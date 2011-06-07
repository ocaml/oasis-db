
open Lwt_log
open ODBContext

let debug ~ctxt fmt = 
  debug_f ~logger:ctxt.logger ~section:ctxt.section fmt 

let info ~ctxt fmt =
  info_f ~logger:ctxt.logger ~section:ctxt.section fmt

let warning ~ctxt fmt =
  warning_f ~logger:ctxt.logger ~section:ctxt.section fmt

let error ~ctxt fmt =
  error_f ~logger:ctxt.logger ~section:ctxt.section fmt

let () =
  Printexc.register_printer
    (function
       | Unix.Unix_error (e, f, a) ->
           Some (Printf.sprintf "Unix.Unix_error (%s, %S, %S)"
                   (Unix.error_message e) f a)
       | e ->
           try 
             Some (OASISMessage.string_of_exception e)
           with e -> 
             None)
