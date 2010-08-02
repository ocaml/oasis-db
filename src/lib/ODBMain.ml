
(** Main loop for OASIS-DB backend 
  *)

open Lwt
open ODBGettext
open ODBMessage

let run = 
  let catch_exn ~ctxt (nm, f) = 
    catch 
      (f ~ctxt)
      (function
        | e ->
            begin
              let e_str = 
                try 
                  string_of_exception e
                with 
                | Failure str ->
                    str
                | e ->
                    Printexc.to_string e
              in
                error ~ctxt
                  (f_ "Function %s ends with exception: %s")
                  nm e_str
            end)
  in

    ODBRunner.singleton
      "ODBMain.run"
      (fun ~ctxt () ->
         try 
           OASISBuiltinPlugins.init ();
           join 
             (List.map
                (catch_exn ~ctxt)
                [
                  "ODBStorage.run",  ODBStorage.run;
                  "ODBIncoming.run", ODBIncoming.run;
                ])
         with e ->
           fail e)

