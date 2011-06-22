
(** Support for deriving operations
  * @author Sylvain Le Gall
  *)

open OASISVersion

let () = ignore "(*"
let rex_oasis_ver = Pcre.regexp "(.*)oasis([0-9\\.]+)"

let version lst ver = 
  let ver_incr lst = 
    match List.rev lst with 
      | hd :: tl ->
          List.rev (hd + 1 :: tl)
      | [] ->
          assert false
  in

  let extract_oasis_ver ver_str = 
    try 
      let substr = 
        Pcre.exec ~rex:rex_oasis_ver ver_str
      in
      let main_ver  = 
        Pcre.get_substring substr 1 
      in
      let oasis_ver = 
        List.map 
          int_of_string
          (ExtLib.String.nsplit 
             (Pcre.get_substring substr 2) 
             ".")
      in
        main_ver, oasis_ver
    with Not_found ->
      ver_str, [0]
  in

  let string_of_oasis_ver v = 
    String.concat "." (List.map string_of_int v)
  in

  let compare_oasis_ver v1 v2 =
    OASISVersion.version_compare 
      (version_of_string (string_of_oasis_ver v1))
      (version_of_string (string_of_oasis_ver v2))
  in

  let rec find_in_between_ver cur_oasis_ver next_oasis_ver =
    let () = 
      assert(compare_oasis_ver cur_oasis_ver next_oasis_ver < 0)
    in
    let just_incr = ver_incr cur_oasis_ver in
      if compare_oasis_ver just_incr next_oasis_ver < 0 then
        just_incr
      else
        find_in_between_ver (cur_oasis_ver @ [0]) next_oasis_ver 
  in

  let next_ver_opt : OASISVersion.t option = 
    let rec find' =
      function
        | hd1 :: hd2 :: tl when hd1 = ver ->
            Some hd2
        | _ :: tl ->
            find' tl 
        | [] ->
            None
    in
      find' lst
  in

  let main_ver, cur_oasis_ver =
    extract_oasis_ver (string_of_version ver)
  in

  let main_ver, oasis_ver = 
    match next_ver_opt with 
      | Some next_ver ->
          let next_main_ver, next_oasis_ver =
            extract_oasis_ver (string_of_version next_ver)
          in
            if next_main_ver <> main_ver then
              main_ver, (ver_incr cur_oasis_ver)
            else
              main_ver, find_in_between_ver cur_oasis_ver next_oasis_ver 

      | None ->
          main_ver, (ver_incr cur_oasis_ver)
  in

  let new_ver_str = 
     Printf.sprintf "%soasis%s"
       main_ver
       (string_of_oasis_ver oasis_ver)
  in
    version_of_string new_ver_str
