
open Eliom_services
open Eliom_parameters
open XHTML.M
open Eliom_predefmod.Xhtml
open OASISVersion
open ODBPkgVer
open ODBGettext

type version_option = 
  | NoVersion
  | Version of OASISVersion.t
  | LatestVersion

module ExtParams = 
struct

  open Eliom_parameters

  let version = 
    user_type
      ~of_string:version_of_string
      ~to_string:string_of_version

  let version_option =
    user_type 
      ~of_string:
      (function
         | "" -> 
             NoVersion
         | "latest" -> 
             LatestVersion
         | str -> 
             Version (version_of_string str))
      ~to_string:
      (function
         | NoVersion -> 
             ""
         | LatestVersion ->
             "latest"
         | Version ver ->
             string_of_version ver)

end

exception Timeout of string
exception RequiresAuth
exception InsufficientAuth
exception StateTransitionNotAllowed

let home       = new_service ["home"] unit ()
let browse     = new_service ["browse"] unit () 
let upload     = new_service ["upload"] unit ()
let contribute = new_service ["contribute"] unit ()
let about      = new_service ["about"] unit ()

let view = 
  new_service ["view"] 
    (suffix 
       (string "pkg" ** 
        ExtParams.version_option "ver")) 
    ()

let my_account =
  new_service ["my_account"] 
    (opt (string "redirect"))
    ()

let new_account = 
  new_service ["new_account"]
    (opt (string "redirect"))
    ()

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

(** Split into n-column, using table 
  *)
let ncol_table ?a ?(mk_table=odd_even_table ?caption:None ?columns:None) n empty_tr lst =
  let () =
    if n <= 0 then
      invalid_arg "ncol_table(0)"
  in

  let rec split prev_acc hd_acc next_acc lst = 
    let e, tl =
      match lst with
        | hd :: tl -> hd, tl
        | [] -> empty_tr, []
    in
    let prev_acc = 
      (e :: hd_acc) :: prev_acc
    in
      match next_acc, tl with
        | hd_acc :: next_acc, _ ->
            split prev_acc hd_acc next_acc tl
        | [], [] ->
            List.rev_map List.rev prev_acc

        | [], tl ->
            begin 
              match List.rev prev_acc with 
                | hd_acc :: next_acc ->
                    split [] hd_acc next_acc tl
                | [] ->
                    invalid_arg "ncol_table(1)"
            end
  in
  
  let col_lst =
    split 
      [] (* prev *)
      [] (* cur *)
      (Array.to_list (Array.make (n - 1) [])) (* next *)
      lst
  in

  let table =
    match col_lst with 
      | hd :: tl ->
          let classe =
            Printf.sprintf 
              "col-%02dpercent"
              (100 / n)
          in
          let mk_td lst =
            let ctnt =
              match lst with 
                | hd :: tl ->
                    mk_table hd tl
                | [] ->
                    invalid_arg "ncol_table(3)"
            in
              td ~a:[a_class [classe]] [ctnt]
          in
            table 
              ?a
              (tr
                 (mk_td hd)
                 (List.map mk_td tl))
              []

      | [] ->
          invalid_arg "ncol_table(4)"
  in
    table
        

let rec html_flatten sep =
  function
    | e1 :: ((_ :: _) as tl) ->
        e1 @ (sep :: (html_flatten sep tl))
    | [e] ->
        e
    | [] ->
        []

let rec html_concat sep =
  function
    | e1 :: ((_ :: _) as tl) ->
        e1 :: sep :: (html_concat sep tl)
    | [_] | [] as lst ->
        lst

let non_zero_lst_html id nm lst = 
  let len =
    List.length lst 
  in
    if len > 0 then
      Some (id, nm len, html_concat (pcdata (s_ ", ")) lst)
    else
      None

let non_zero_lst id nm lst =
  non_zero_lst_html id nm (List.map pcdata lst)

(* Field for versions number and their links to 
 * version's page
 *)
let versions_field ~sp pkg_ver_lst pkg_ver_cur_opt pkg_ver_latest =
  let mk_pcdata pkg_ver = 
    if pkg_ver = pkg_ver_latest then
      b [pcdata ((string_of_version pkg_ver.ver)^"*")]
    else
      pcdata (string_of_version pkg_ver.ver)
  in
  let lst =
    List.fold_left
      (fun acc pkg_ver ->
         let hd = 
           if pkg_ver_cur_opt = Some pkg_ver then
             mk_pcdata pkg_ver
           else
             a
               view
               sp
               [mk_pcdata pkg_ver]
               (pkg_ver.pkg, Version pkg_ver.ver)
         in
           hd :: acc)
      []
      pkg_ver_lst
  in
  let rec add_comma =
    function
      | e1 :: e2 :: tl ->
          e1 :: pcdata (s_ ", ") :: add_comma (e2 :: tl)
      | _ :: [] 
      | [] as lst ->
          lst
  in
    add_comma (List.rev lst)

