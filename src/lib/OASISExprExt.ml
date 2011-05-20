
(** Extensions for OASISExpr
   
    @author Sylvain Le Gall
  *)

open OASISTypes
open OASISUtils
open ODBGettext
open OASISExpr

let var_get_flags flags nm = 
  match MapString.find nm flags with
    | Some b -> string_of_bool b
    | None -> raise Not_found

let rec solve_flags pkg = 
  let flags_default, flags_all = 
    (* Gather all flags *)
    List.fold_left
      (fun ((flags_default, flags_all) as acc) -> 
         function
           | Flag (cs, flag) ->
               MapString.add cs.cs_name flag.flag_default flags_default,
               SetString.add cs.cs_name flags_all
           | Library _ | Executable _ | Test _ | Doc _ | SrcRepo _ ->
               acc)
      (MapString.empty, SetString.empty)
      pkg.sections
  in
  let rec extract_flag acc =
    function
      | EFlag nm -> SetString.add nm acc
      | EBool _  | ETest _ -> acc
      | ENot e -> extract_flag acc e
      | EAnd (e1, e2) | EOr (e1, e2) ->
          extract_flag (extract_flag acc e1) e2
  in

  (* Try to solve an expression *)
  let rec solve_one_expr flags_solved expr flags_unsolved = 
    (* Solve the dependencies of the expression *)
    let flags_dep = 
      extract_flag SetString.empty expr 
    in
    let flags_dep_unsolved =
      SetString.inter flags_dep flags_unsolved
    in
    let flags_solved =
      solve_all flags_solved flags_dep_unsolved
    in
    let res =
      trivial_eval ~flags:flags_solved pkg expr
    in
      flags_solved, res, flags_unsolved

  (* Try to solve a flag and add its result to flags_solved *)
  and solve_one flags_solved flag_nm default flags_unsolved = 
    match default with 
      | (expr, vl) :: tl ->
          begin
            let flags_solved, res, flags_unsolved = 
              solve_one_expr flags_solved expr flags_unsolved
            in
              match res with 
                | Some true ->
                    MapString.add flag_nm (Some vl) flags_solved,
                    flags_unsolved
                | Some false ->
                    solve_one flags_solved flag_nm tl flags_unsolved
                | None ->
                    MapString.add flag_nm None flags_solved,
                    flags_unsolved
          end

      | [] ->
          MapString.add flag_nm None flags_solved, flags_unsolved

  and solve_all flags_solved flags_unsolved =
    if flags_unsolved <> SetString.empty then
      begin
        let flag_nm =
          SetString.choose flags_unsolved 
        in
        let flags_unsolved = 
          SetString.remove flag_nm flags_unsolved 
        in
        let default = 
          MapString.find flag_nm flags_default
        in
        let flags_solved, flags_unsolved =
          solve_one flags_solved flag_nm default flags_unsolved 
        in
          solve_all flags_solved flags_unsolved
      end
    else
      begin
        flags_solved
      end
  in

    solve_all MapString.empty flags_all




(** Return the value of expression, in the context of the package
  * if it is not possible to evaluate the value, return None.
  *)
and trivial_eval ?flags pkg expr = 
  let flags = 
    match flags with 
      | Some flg -> flg
      | None -> solve_flags pkg
  in
    try
      let res = 
        eval (var_get_flags flags) expr
      in
        Some res

    with _ ->
      None

(** See {!OASISExpr.choose}, use {trivial_eval}
  *)
let trivial_choose ?printer ?name pkg lst = 
  let flags =
    solve_flags pkg
  in
    try 
      let res = 
        choose ?printer ?name (var_get_flags flags) lst
      in
        Some res

    with _ ->
      None

