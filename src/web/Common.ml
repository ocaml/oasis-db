
open Eliom_services
open Eliom_parameters
open XHTML.M

exception Timeout of string
exception RequiresAuth

let home       = new_service ["home"] unit ()
let browse     = new_service ["browse"] (opt (string "pkg") ** opt (string "ver")) () 
let upload     = new_service ["upload"] unit ()
let contribute = new_service ["contribute"] unit ()
let about      = new_service ["about"] unit ()


let browse_all = preapply browse (None, None)

(** Set style row in a table to be odd and even
  *)
let odd_even_table ?caption ?columns hd tl = 
  let rec set_class is_odd =
    function
      | e :: tl -> 
          let css_class =
            if is_odd then "odd" else "even"
          in
            addto_class1 css_class e :: set_class (not is_odd) tl

      | [] -> 
           []
  in
    table 
      ?caption 
      ?columns 
      (addto_class1 "odd" hd)
      (set_class false tl)
        


