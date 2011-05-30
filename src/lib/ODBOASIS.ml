
open Lwt

let from_string ~ctxt ?fn str = 
  try 
    let pkg = 
      OASISParse.from_string 
        ~ctxt:{(ODBContext.to_oasis ctxt) with 
                  OASISContext.ignore_plugins = true;
                  OASISContext.ignore_unknown_fields = true;}

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

let from_chn ~ctxt ~fn chn =
  LwtExt.IO.with_file_content_chn ~fn chn
  >>= 
  from_string ~ctxt ~fn

