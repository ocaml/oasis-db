
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

let string_of_exception e =
  try 
    OASISMessage.string_of_exception e
  with e ->
    raise e 
