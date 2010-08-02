
open Lwt

let ctxt = 
  ODBContext.default

let rec tic_tac () =
  let aux str () =
    Lwt_io.printl str
    >>= fun () ->
    Lwt_unix.sleep 1.0
  in
    aux "tic" () >>= aux "tac" >>= tic_tac

let () = 
  Lwt_main.run
    (join
        [
          (* Backend engine *)
          ODBMain.run ~ctxt ();

          (* Debug code *)
          ODBStorage.packages () 
          >>= fun lst ->
          Lwt_io.printl ("Packages: "^(String.concat ", " lst))
          >>= fun () ->
          tic_tac ()
          >>= fun () -> 
          Lwt_io.printl "This is the end!"
        ])


