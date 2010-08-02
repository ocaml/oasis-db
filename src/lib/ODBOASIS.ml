
open Lwt

let from_file ~ctxt ?ignore_plugins fn =
  LwtExt.IO.with_file_content fn
  >>= fun str ->
  begin
    try 
      let pkg = 
        OASISParse.from_string 
          ~ctxt:(ODBContext.to_oasis ctxt)
          ?ignore_plugins
          ~fn:fn
          str
      in
        return pkg
    with e ->
      fail e
  end
