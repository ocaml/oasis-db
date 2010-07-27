
(** Guess package/version of an archive
    @author Sylvain Le Gall
  *)

open Lwt 
open ODBGettext

(** {2 Utils} *)

type name = string
type version = string

let pkg_regexp_str = 
  "[\\w\\-]+?"

let ver_regexp_str = 
  "[\\d]+(\\.[\\d]+)*(\\~[\\w]+[\\d]*)?"

(** Extract package/version from a tarball or topdir name
  *)
let tarname_regexp = 
  let regexp = 
    Pcre.regexp
      ("^("^pkg_regexp_str^")(?:[-_]release)?[-_]("^ver_regexp_str^")$")
  in

    fun q nm ->
      try 
        let substrs = 
          Pcre.exec ~rex:regexp nm
        in
          [
            q,
            Pcre.get_substring substrs 1,
            Pcre.get_substring substrs 2;
          ]
      with Not_found ->
        []

(** Define the top directory
  *)
let topdir ?section ?logger fn dn = 
  ODBFileUtil.fold_dir 
   (fun full base (acc, count) ->
     return 
      (if Sys.is_directory full then 
         Some (full, base), count + 1
       else
         acc, count + 1))
   dn
   (None, 0)
   >>= fun (res, count) ->
   if count <> 1 then
     Lwt_log.warning_f ?section ?logger 
        (fn_ 
          "Archive '%s' contains %d top level file/directory."
          "Archive '%s' contains %d top level files/directories."
          count)
        fn count
      >>= fun () ->
      return res
   else
     return res

(** Search files in the top directory and run a function on them.
  *)
let find ?section ?logger fn dn files f = 
  topdir ?section ?logger fn dn
  >>= function
    | Some (topdir, _) ->
        (** Find configure.{in,ac} *)
        ODBFileUtil.fold_dir 
          (fun full base acc ->
            if List.mem base files then
              f full acc
            else 
              return acc)
          topdir
          []

    | None ->
        return []

(** Search files in the top directory and scan their contents.
  *)
let find_and_scan ?section ?logger fn dn files scan = 
  find 
    ?section 
    ?logger 
    fn 
    dn 
    files 
    (fun full acc -> 
      Lwt_stream.fold
        scan
        (Lwt_io.lines_of_file full)
        acc)

(** {2 Package/version completion methods}
 *)

(** Extract version from tarball name *)
let tarball_name ?section ?logger fn an dn = 
  Lwt_log.debug_f 
    ?section ?logger
    (f_ "Try to match tarname '%s'")
    an 
  >>= fun () ->   
  return (tarname_regexp 0.9 an)

(** Extract version from top level directory *)
let topdir_name ?section ?logger fn an dn =
  topdir ?section ?logger fn dn 
  >>= function
    | Some (full, base) ->
        return (tarname_regexp 0.8 base)
    | None ->
        return []

(** Extract data from configure.ac/configure.in files *)
let configure_ac ?section ?logger fn an dn =
  let ac_init_rgxp = 
    Pcre.regexp
      ("AC_INIT\\(\\s*("^pkg_regexp_str^")\\s*,\\s*("^ver_regexp_str^")")
  in
  let ac_init_extract line acc = 
    try 
      let substrs =
        Pcre.exec ~rex:ac_init_rgxp line
      in
        (0.7,
         Pcre.get_substring substrs 1,
         Pcre.get_substring substrs 2)
        :: acc
    with Not_found ->
      acc
  in
    find_and_scan 
      fn dn
      ?section ?logger
      ["configure.ac"; "configure.in"]
      ac_init_extract

(** Extract data from META files *)
let meta ?section ?logger fn an dn =
  let meta_ver_rgxp =
    Pcre.regexp 
      ("version\\s*=\\s*\"("^ver_regexp_str^")\"")
  in
  let meta_extract line acc = 
    try 
      let substrs = 
        Pcre.exec ~rex:meta_ver_rgxp line
      in
        (* TODO: separate pkg_name/pkg_ver extraction *)
        (0.6,
         "toto",
         Pcre.get_substring substrs 1)
        :: acc
    with Not_found ->
      acc
  in
    (* TODO: this only apply to topdir, look deeper in the the 
       filesystem.
     *)
    find_and_scan 
      fn dn
      ?section ?logger
      ["META"]
      meta_extract

exception OASISFileTooBig of int64 * string
open OASISTypes

(** Extract data from OASIS file *)
let oasis ?section ?logger fn an dn = 
  let oasis_extract fn acc =
    LwtExt.IO.with_file_content fn
    >>= fun str ->
    begin
      let pkg = 
        OASISParse.from_string 
          (* TODO: redirect context to logger *)
          ~ctxt:!OASISContext.default
          ~ignore_unknown:true
          ~fn:fn
          str
      in
        return ((1.0, pkg.name, OASISVersion.string_of_version pkg.version) :: acc)
    end
  in
    find
      fn dn
      ?section ?logger
      ["_oasis"]
      oasis_extract

(** {2 Completion} *)

module MapString = Map.Make(String)

(** Merge a list of tuples to find what the best guess
    for pkg/ver
  *)
let merge lst = 
  let sort_choices lst = 
    let rec merge mp =
      function
        | (q, ans) :: tl ->
            begin
              let q = 
                try 
                  max q (MapString.find ans mp)
                with Not_found ->
                  q
              in
                merge (MapString.add ans q mp) tl
            end

        | [] ->
            mp
    in
    let mp =
      merge MapString.empty lst
    in
    let lst = 
      MapString.fold
        (fun ans q acc -> (q, ans) :: acc)
        mp
        []
    in
      List.rev_map snd
        (List.sort compare lst)
  in
    sort_choices (List.rev_map (fun (q, pkg, _) -> q, pkg) lst),
    sort_choices (List.rev_map (fun (q, _, ver) -> q, ver) lst)

(** TODO: move to unit testing 
let test () = 
  List.iter
    (fun an -> 
       let lst = 
         tarname_regexp 0.8 an
       in
         Printf.printf "tarname: %s\n%!" an;
         List.iter 
           (fun (q, pkg, ver) ->
              Printf.printf "q: %f; pkg: %s; ver: %s\n%!" 
                q pkg ver)
           lst)
    [
      "foo-0.1.0";
      "bar-0.2.0";
      "baz_3.0~alpha1";
      "sexplib310-release-5.1.0";
    ]
    *)

let run ?section ?logger fn an dn =
  let lst = 
    [
      "tarball_name", tarball_name;
      "topdir_name",  topdir_name;
      "configure_ac", configure_ac;
      "meta",         meta;
      "oasis",        oasis;
    ]
  in
    (* TODO: catch, log and ignore exceptions *)
    Lwt_list.map_s
      (fun (nm, f) ->
        Lwt_log.debug_f ?section ?logger 
          (f_ "Running package/version completion %s on '%s'")
          nm fn
        >>= fun () ->
        f ?section ?logger fn an dn
        >>= fun lst ->
        Lwt_list.iter_s 
         (fun (q, pkg, ver) ->
           Lwt_log.debug_f
             ?section ?logger
             (f_ "Result of %s: q: %f; pkg: %s; ver: %s")
             nm q pkg ver)
         lst
        >>= fun () ->
        return lst)
      lst
    >>= fun lst' ->
    return (merge (List.flatten lst'))

(** TODO: load _oasis file, guess order *)
