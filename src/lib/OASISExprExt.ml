
(** Extensions for OASISExpr
   
    @author Sylvain Le Gall
  *)

open OASISTypes
open OASISUtils
open ODBGettext
open OASISExpr

exception Unsolved_variable of string

type t = package

(** Knowing the given data, try to solve the choice. *)
let trivial_choose_choice ?printer ?name data lst = 
  try 
    Some 
      (choose 
         ?printer ?name
         (fun nm ->
            try
              MapString.find nm data 
            with Not_found ->
              raise (Unsolved_variable nm))
         lst)
  with (Unsolved_variable _) ->
    None

(** When possible solve a flag and add its value to known data. *)
let solve_flags data pkg = 
  List.fold_left
    (fun data ->
       function 
         | Flag (cs, flg) ->
             begin
               match trivial_choose_choice data flg.flag_default with 
                 | Some b -> 
                     MapString.add cs.cs_name (string_of_bool b) data 
                 | None ->
                     data
             end
         | Library _ | Executable _ | SrcRepo _ | Test _ | Doc _ ->
             data)
    data pkg.sections


(** See {!OASISExpr.choose}. *)
let trivial_choose ?printer ?name preset_data pkg lst = 
  let data = 
    List.fold_left
      (fun flags (flg, vl) ->
         MapString.add flg vl flags)
      MapString.empty
      preset_data
  in
  let data =
    solve_flags data pkg
  in
    trivial_choose_choice ?printer ?name data lst

(** Search for an expression inside another expression. *)
let rec contains ~what expr = 
  if expr = what then
    true
  else
    match expr with 
      | EAnd (e1, e2) | EOr (e1, e2) ->
          (contains ~what e1) || (contains ~what e2)
      | ENot e ->
          contains ~what e
      | EBool _ | EFlag _ | ETest _ ->
          false

let choice_contains ~what lst =
  List.exists (fun (e, _) -> contains ~what e) lst
