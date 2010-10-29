
open Eliom_services
open Eliom_parameters

exception Timeout of string
exception RequiresAuth

let home       = new_service ["home"] unit ()
let browse     = new_service ["browse"] (opt (string "pkg") ** opt (string "ver")) () 
let upload     = new_service ["upload"] unit ()
let contribute = new_service ["contribute"] unit ()
let about      = new_service ["about"] unit ()


let browse_all = preapply browse (None, None)
