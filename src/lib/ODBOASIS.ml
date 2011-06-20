
open OASISTypes
open Lwt
open ODN

let from_string ~ctxt ?printf ?fn str = 
  let ctxt = ODBContext.to_oasis ctxt in
  let ctxt = 
    {ctxt with 
         OASISContext.
         ignore_plugins = true;
         ignore_unknown_fields = true;
         printf = 
           (match printf with 
              | Some f -> f
              | None -> ctxt.OASISContext.printf)}
  in
    OASISParse.from_string ~ctxt ?fn str

let from_string_lwt ~ctxt ?fn str = 
  try 
    return (from_string ~ctxt ?fn str)
  with e ->
    fail e

let from_file ~ctxt fn =
  LwtExt.IO.with_file_content fn
  >>=
  from_string_lwt ~ctxt ~fn

let from_chn ~ctxt ~fn chn =
  LwtExt.IO.with_file_content_chn chn
  >>= 
  from_string_lwt ~ctxt ~fn

(** Compare two packages and tell if some build sensitive
  * information has been changed
  *)
let check_build_sensitive oasis1 oasis2 =
  let oasis_ref = oasis1 in
  let norm oasis = 
    let oasis =
      (* Normalize toplevel fields *)
      {oasis with 
           license = oasis_ref.license;
           license_file = oasis_ref.license_file;
           copyrights   = oasis_ref.copyrights;
           maintainers  = oasis_ref.maintainers;
           authors      = oasis_ref.authors;
           homepage     = oasis_ref.homepage;
           synopsis     = oasis_ref.synopsis;
           description  = oasis_ref.description;
           categories   = oasis_ref.categories}
    in
    (* Normalize sections *)
    let sections = 
      List.fold_left
        (fun acc sct ->
           let sct_ref = 
             let sct_id = OASISSection.section_id sct in
               try 
                 Some 
                   (OASISSection.section_find 
                      sct_id 
                      oasis_ref.sections)
               with Not_found ->
                 None
           in
             match sct with 
               | Library _ 
               | Executable _ 
               | Test _ ->
                   sct :: acc

               | Doc (cs, doc) ->
                   begin
                     match sct_ref with 
                       | Some (Doc (_, doc_ref)) ->
                           Doc
                             (cs, 
                              {doc with 
                                   doc_title   = doc_ref.doc_title;
                                   doc_authors = doc_ref.doc_authors;
                                   doc_abstract = doc_ref.doc_abstract})
                           :: acc
                       | _ ->
                           sct :: acc
                   end

               | Flag (cs, flag) ->
                   begin
                     match sct_ref with 
                       | Some (Flag (cs_ref, flag_ref)) ->
                           Flag
                             (cs,
                              {flag with 
                                   flag_description = flag_ref.flag_description})
                           :: acc

                       | _ ->
                           sct :: acc 
                   end

               | SrcRepo _ ->
                   acc)
        []
        oasis.sections
    in
      {oasis with sections = List.rev sections}
  in

  let diff_changed path acc t1 t2 =
    true, 
    (path, 
     `Changed (string_of_odn t1, 
               string_of_odn t2)) 
    :: acc
  in

  (* Do a structural comparison of ODN.t to find fields that don't match 
   *)
  let rec diff path acc (t1, t2)=
    match (t1, t2) with 
      | REC (nm1, lst1), REC (nm2, lst2) ->
          begin
            if nm1 <> nm2 then
              diff_changed path acc t1 t2
            else
              let sort = 
                List.sort 
                  (fun (s1, _) (s2, _) -> String.compare s1 s2)
              in
                diff_list_fields path acc ((sort lst1), (sort lst2))
          end

      | LST lst1, LST lst2 ->
          diff_list_ord path acc (lst1, lst2)

      | VRT (vrn1, lst1), VRT (vrn2, lst2) ->
          if vrn1 <> vrn2 then
            diff_changed path acc t1 t2
          else
            diff_list_ord path acc (lst1, lst2)

      | TPL lst1, TPL lst2 ->
          diff_list_ord path acc (lst1, lst2)

      | APP (nm1, lst_arg1, lst_data1), APP (nm2, lst_arg2, lst_data2) ->
          if nm1 <> nm2 then 
            diff_changed path acc t1 t2 
          else
            (* TODO: deeper inspection *)
            begin
              false, acc
            end

      | PVR (vnm1, opt1), PVR (vnm2, opt2) ->
          if vnm1 = vnm2 then
            match opt1, opt2 with 
              | Some t1, Some t2 ->
                  diff path acc (t1, t2) 
              | None, None ->
                  false, acc
              | _, _ ->
                  diff_changed path acc t1 t2
          else
            diff_changed path acc t1 t2

      | t1, t2 ->
          if t1 <> t2 then
            diff_changed path acc t1 t2
          else
            false, acc


  and diff_list_ord path acc (lst1, lst2) =
    try 
      List.fold_left2
        (fun (res, acc) e1 e2 ->
           let is_diff, acc = 
             diff path acc (e1, e2)
           in
             is_diff || res, acc)
        (false, acc)
        lst1 lst2
    with 
      | Invalid_argument _ ->          
          diff_changed path acc (LST lst1) (LST lst2)
      | e ->
          raise e
  and diff_list_fields path acc =
    function
      | (((nm1, t1) :: tl1) as lst1), (((nm2, t2) :: tl2) as lst2) ->
          begin
            let cmp = String.compare nm1 nm2 in
              if cmp = 0 then
                let is_diff, acc = diff (nm1 :: path) acc (t1, t2) in
                let is_diff', acc = diff_list_fields path acc (tl1, tl2) in
                  is_diff || is_diff', acc

              else if cmp < 0 then
                let _, acc = 
                  diff_list_fields path ((nm1 :: path, `Missing) :: acc) (tl1, lst2)
                in
                  true, acc

              else 
                let _, acc =
                  diff_list_fields path ((nm2 :: path, `Added) :: acc) (lst1, tl2)
                in
                  true, acc
          end
      | [], [] ->
          begin
            false, acc
          end
      | (nm, _) :: tl, [] ->
          begin
            let _, acc =
              diff_list_fields path ((nm :: path, `Missing) :: acc) (tl, [])
            in
              true, acc
          end
      | [], (nm, _) :: tl ->
          begin
            let _, acc =
              diff_list_fields path ((nm :: path, `Added) :: acc) ([], tl)
            in
              true, acc
          end
  in

  let oasis1_odn = odn_of_package (norm oasis1) in
  let oasis2_odn = odn_of_package (norm oasis2) in

  let _, msg =
    diff [] [] (oasis1_odn, oasis2_odn)
  in
    List.rev_map 
      (fun (path, ev) ->
         String.concat "." (List.rev path), ev)
      msg




