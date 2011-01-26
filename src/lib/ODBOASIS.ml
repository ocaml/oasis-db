
open Lwt

let from_string ~ctxt ?fn str = 
  try 
    let pkg = 
      OASISParse.from_string 
        ~ctxt:{(ODBContext.to_oasis ctxt) with 
                  OASISContext.ignore_plugins = true}
        ?fn
        str
    in
      return pkg
  with e ->
    fail e

let from_file ~ctxt fn =
  LwtExt.IO.with_file_content fn
  >>=
  from_string ~ctxt ~fn
