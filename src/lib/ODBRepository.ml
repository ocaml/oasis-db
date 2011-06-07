
(** Describe a repository where packages can be uploaded and downloaded.
  
    @author Sylvain Le Gall
  *)

open Inifiles
open OASISUtils
open ODBGettext
open ODBUtils
open Sexplib.Conv

TYPE_CONV_PATH "ODBRepository"

type uri = string with sexp

type t = 
  {
    repo_name:            string;
    repo_long_name:       string option;
    repo_description:     string option;
    repo_priority:        int;
    repo_dist_uri:        uri;
    repo_incoming_uri:    uri option;
    repo_api_uri:         uri option;
    repo_download_policy: [`Nothing | `Minimal | `Full];
  } with sexp 

type vt = V1 of t with sexp 

let upgrade =
  function
    | V1 t -> t

let string_of_download_policy =
  function 
    | `Nothing -> "nothing"
    | `Minimal -> "minimal"
    | `Full    -> "full"

let uri_pat =
  "(\\w+:{0,1}\\w*@)?(\\S+)(:[0-9]+)?(/|/([\\w#!:.?+=&%@!\\-/]))?"

let uri_regexp = 
  Pcre.regexp uri_pat

let name_pat = 
  "[a-z]+"

(** Specification for repository section. 
  *)
let ini_repo_specs = 
  [
    {
      sec_name = "repo";
      sec_required = false;
      sec_attributes =
        [
          {
            atr_name      = "descrs";
            atr_required  = false;
            atr_default   = None;
            atr_validator = Some uri_regexp;
          };

          {
            atr_name      = "priority";
            atr_required  = false;
            atr_default   = None;
            atr_validator = Some (Pcre.regexp (name_pat^" *[0-9]+"));
          };

          {
            atr_name      = "download_policy";
            atr_required  = false;
            atr_default   = None;
            atr_validator = Some (Pcre.regexp (name_pat^" *(nothing|minimal|full)"));
          };
        ];
    }
  ]

let in_cache_descrs cache_dir uri =
  let fn_of_uri uri =
    let buf = 
      Buffer.create (String.length uri)
    in
      String.iter 
        (function
           | 'A'..'Z' | 'a'..'z' | '0'..'9' as c ->
               Buffer.add_char buf c 
           | _ ->
               Buffer.add_char buf '_')
        uri;
      Buffer.contents buf
  in
    FilePath.make_filename
      [cache_dir; "descrs"; (fn_of_uri uri)]

let in_cache_repos cache_dir t =
  FilePath.make_filename
    [cache_dir; "repos"; t.repo_name]

let fold_descrs ini f (msgs, acc) = 
  try 
    List.fold_left
      (fun (msgs, acc) uri ->
         if Pcre.pmatch ~rex:uri_regexp uri then
           begin
             f (msgs, acc) uri
           end
         else
           begin
             ((`Error 
                 (Printf.sprintf 
                    (f_ "Not a repository URI '%s'")
                    uri)) :: msgs), acc
           end)
      (msgs, acc)
      (ini#getaval "repo" "descrs")
  with 
    | Invalid_section "repo" ->
        ((`Error (s_ "No [repo] section defined")) :: msgs), acc
    | Invalid_element "descrs" ->
        ((`Error (s_ "No repo.descrs URI defined")) :: msgs), acc
    | e -> 
        raise e

let init_common cache_dir = 
  List.iter 
    (fun dn ->
       FileUtil.mkdir ~parent:true 
         (FilePath.make_filename [cache_dir; dn]))
    ["descrs"; "repos"]

(* Load repository data from cache_dir and set the priority/download policy
 * override according to a repo sections.
 *)
let load cache_dir ini =
  let () = 
    init_common cache_dir
  in

  let regexp_repo_name = 
    ignore "(*";
    Pcre.regexp ("("^name_pat^") *(.*)") 
  in

  let overrides = 
    let extract atr f = 
      let f' mp str = 
        let substr = 
          Pcre.exec ~rex:regexp_repo_name str
        in
        let nm = 
          Pcre.get_substring substr 1
        in
        let vl =
          Pcre.get_substring substr 2
        in
          MapString.add nm (f vl) mp
      in
        try 
            List.fold_left
              f' 
              MapString.empty
              (ini#getaval "repo" atr)
        with 
          | Invalid_section "repo" ->
              MapString.empty
          | Invalid_element nm when nm = atr ->
              MapString.empty
          | e ->
              raise e
    in
    let mp_priority =
      extract "priority" 
        (fun s -> 
           try 
             int_of_string s
           with e ->
             failwith 
               (Printf.sprintf
                  (f_ "Priority '%s' is not an integer")
                  s)) 
    in
    let mp_download_policy =
      extract "download_policy" 
        (function
           | "nothing" -> `Nothing
           | "minimal" -> `Minimal 
           | "full"    -> `Full
           | s -> 
               failwith
                 (Printf.sprintf 
                    (f_ "Unknown download policy '%s'")
                    s))
    in
      mp_priority, mp_download_policy
  in

  let apply_overrides uri fn t (msgs, (acc, (mp_priority, mp_download_policy))) = 
    let set_override vl mp = 
      try 
        MapString.find t.repo_name mp
      with Not_found ->
        vl
    in
    let t =
      {t with 
           repo_priority = 
             set_override t.repo_priority mp_priority;
           repo_download_policy =
             set_override t.repo_download_policy mp_download_policy;}
    in
      msgs, 
      (((uri, fn, t) :: acc),
       (MapString.remove t.repo_name mp_priority,
        MapString.remove t.repo_name mp_download_policy))
  in

  let (msgs, (acc, (mp_priority, mp_download_policy))) =
    fold_descrs ini
      (fun (msgs, (acc, overrides)) uri ->
         let fn = 
           in_cache_descrs cache_dir uri
         in
           try 
             let t = 
               upgrade (vt_of_sexp (Sexplib.Sexp.load_sexp fn))
             in
               try
                 let uri', fn', t' = 
                   List.find 
                     (fun (_, _, t') -> t'.repo_name = t.repo_name) 
                     acc
                 in
                   ((`Error 
                       (Printf.sprintf 
                          (f_ "Repo '%s' is defined at the same time by URI '%s' (%s) and '%s' (%s)")
                          t.repo_name uri fn uri' fn')) :: msgs), 
                   (acc, 
                    overrides)
               with Not_found ->
                 begin
                   if t.repo_name = "local" then
                     ((`Error
                         (Printf.sprintf
                            (f_ "Repo 'local' cannot be defined by external URI '%s' (%s)")
                            uri fn)) :: msgs), 
                     (acc,
                      overrides)
                   else
                     begin
                       apply_overrides uri fn t (msgs, (acc, overrides))
                     end
                 end
           with e ->
             begin
               ((`Error 
                   (Printf.sprintf 
                      (f_ "Cannot load description of URI '%s', file '%s'")
                      uri fn)) :: msgs),
               (acc,
                overrides)
             end)
      ([], ([], overrides))
  in

  let msgs =
    MapString.fold
      (fun k v msgs ->
         (`Warning 
            (Printf.sprintf 
               (f_ "Priority '%s %d' doesn't match any repositories.")
               k v)) :: msgs)
      mp_priority
      msgs
  in

  let msgs =
    MapString.fold
      (fun k v msgs ->
         (`Warning 
            (Printf.sprintf
               (f_ "Download policy '%s %s' doesn't match any repositories.")
               k (string_of_download_policy v))) :: msgs)
      mp_download_policy 
      msgs
  in
  let acc = 
    List.sort 
      (fun (_, _, t1) (_, _, t2) -> t2.repo_priority - t1.repo_priority)
      acc
  in
  let repos = 
    List.map
      (fun (_, _, t) -> t)
      acc
  in
    msgs, repos

(* Download repository description using descrs found in [ini] *)
let update cache_dir ini = 
  let () = 
    init_common cache_dir
  in
  let msgs, () = 
    fold_descrs ini 
      (fun (msgs, ()) uri ->
         try 
           ODBCurl.download_fn uri (in_cache_descrs cache_dir uri);
           (msgs, ())
         with e ->
           (`Error 
              (Printf.sprintf 
                 (f_ "Error while downloading '%s': %s")
                 uri (Printexc.to_string e)) :: msgs), ())
      ([], ())
  in
    msgs

(** Return data as text (for oasis-db website) *)
let content t = 
  Sexplib.Sexp.to_string_hum 
    ~indent:2
    (sexp_of_vt (V1 t)),
  "application/sexp"



