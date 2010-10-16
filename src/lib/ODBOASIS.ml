
open Lwt

let from_file ~ctxt fn =
  LwtExt.IO.with_file_content fn
  >>= fun str ->
  begin
    try 
      let pkg = 
        OASISParse.from_string 
          ~ctxt:{(ODBContext.to_oasis ctxt) with 
                    OASISContext.ignore_plugins = true}
          ~fn:fn
          str
      in
        return pkg
    with e ->
      fail e
  end
