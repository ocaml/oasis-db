
open Lwt

let ctxt = 
  ODBContext.default
    (* TODO: configure *)
    (FilePath.make_filename ["test"; "data"; "storage"])

let () = 
  Lwt_main.run
    ((* Backend engine *)
     ODBMain.run ~ctxt ()
     >>= fun () ->
     (* Debug code *)
     Lwt_io.printl "This is the end!")

