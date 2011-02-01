
(** Guess package/version of an archive
    @author Sylvain Le Gall
  *)

open Lwt 
open ODBGettext
open ODBTypes
open ODBMessage

TYPE_CONV_PATH "ODBCompletion"

(** {2 Types} *)

type 'a answer = 
  | Sure of 'a
  | Unsure of float * 'a
  | NotFound with sexp 

type t =
  {
    ct_pkg:   string answer;
    ct_ver:   version answer;
    ct_ord:   int answer;
    ct_oasis: string option;
  } with sexp

(** {2 Utils} *)

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
          Unsure (q, Pcre.get_substring substrs 1),
          Unsure (q, Pcre.get_substring substrs 2)
      with Not_found ->
        NotFound, NotFound

(** Define the top directory
  *)
let topdir ~ctxt fn dn = 
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
     warning ~ctxt 
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
let find ~ctxt fn dn files f acc = 
  topdir ~ctxt fn dn
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
          acc

    | None ->
        return acc

(** Search files in the top directory and scan their contents.
  *)
let find_and_scan ~ctxt fn dn files scan acc = 
  find ~ctxt  fn dn files 
    (fun full acc -> 
      Lwt_stream.fold_s
        (fun line acc -> scan full line acc)
        (Lwt_io.lines_of_file full)
        acc)
    acc

(** {2 Package/version completion methods}
 *)

(** Extract version from tarball name *)
let tarball_name ~ctxt fn an dn = 
  debug ~ctxt
    (f_ "Try to match tarname '%s'")
    an 
  >>= fun () ->   
  return (tarname_regexp 0.9 an)

(** Extract version from top level directory *)
let topdir_name ~ctxt fn an dn =
  topdir ~ctxt fn dn 
  >>= function
    | Some (full, base) ->
        return (tarname_regexp 0.8 base)
    | None ->
        return (NotFound, NotFound)

let merge_partial_result ~ctxt prev fn v fmt_diff fmt_debug = 
  match prev with 
    | None ->
        begin
          return (Some (fn, v))
        end

    | Some (ref_fn, ref_v) ->
        begin
          if v <> ref_v then
            warning ~ctxt fmt_diff ref_fn ref_v fn v
            >>= fun () ->
            return (Some (fn, v))
          else
            debug ~ctxt fmt_debug fn v
            >>= fun () ->
            return prev
        end

let partial_result q =
  function
    | None -> NotFound
    | Some (_, v) -> Unsure (q, v)

(** Extract data from configure.ac/configure.in files *)
let configure_ac ~ctxt fn an dn =
  let ac_init_rgxp = 
    Pcre.regexp
      ("AC_INIT\\(\\s*("^pkg_regexp_str^")\\s*,\\s*("^ver_regexp_str^")")
  in
  let ac_init_extract fn line ((pkg_prev, ver_prev) as prev) = 
    try 
      let substrs =
        Pcre.exec ~rex:ac_init_rgxp line
      in
        (* Package *)
        merge_partial_result ~ctxt 
          pkg_prev fn (Pcre.get_substring substrs 1)
          (f_ "File '%s' uses package '%s' and '%s' package '%s'")
          (f_ "File '%s' uses package '%s'")
        >>= fun pkg_cur ->
        begin
          (* Version *)
          merge_partial_result ~ctxt
            ver_prev fn (Pcre.get_substring substrs 2)
            (f_ "File '%s' uses version '%s' and '%s' version '%s'")
            (f_ "File '%s' uses version '%s'")
          >>= fun ver_cur ->
          return (pkg_cur, ver_cur)
        end

    with Not_found ->
      return prev
  in
    find_and_scan 
      fn dn
      ~ctxt
      ["configure.ac"; "configure.in"]
      ac_init_extract
      (None, None)
    >>= fun (pkg, ver) ->
    return (partial_result 0.7 pkg, partial_result 0.7 ver)


(** Extract data from META files *)
let meta ~ctxt fn an dn =
  let meta_ver_rgxp =
    Pcre.regexp 
      ("version\\s*=\\s*\"("^ver_regexp_str^")\"")
  in
  let meta_extract fn line prev =
    try 
      let substrs = 
        Pcre.exec ~rex:meta_ver_rgxp line
      in
      let ver_s = 
        Pcre.get_substring substrs 1
      in
        merge_partial_result ~ctxt
          prev fn ver_s 
          (f_ "File '%s' uses version '%s' and '%s' version '%s'")
          (f_ "File '%s' uses version '%s'")

    with Not_found ->
      return prev
  in
    (* TODO: this only apply to topdir, look deeper in the the 
       filesystem.
     *)
    find_and_scan 
      fn dn
      ~ctxt
      ["META"]
      meta_extract
      None
    >>= fun ver ->
    return (NotFound, partial_result 0.6 ver)

open OASISTypes

(** Extract data from OASIS file *)
let oasis ~ctxt fn an dn = 
  let oasis_extract fn _ =
    LwtExt.IO.with_file_content fn 
    >>= fun oasis_content ->
    ODBOASIS.from_string ~ctxt oasis_content
    >>= fun pkg ->
    return 
      (Some oasis_content,
       Sure pkg.name,
       Sure (OASISVersion.string_of_version pkg.version))
  in
    find fn dn ~ctxt
      ["_oasis"]
      oasis_extract
      (None, NotFound, NotFound)

(** {2 Completion} *)

module MapString = Map.Make(String)

(** Merge two possible answers
  *)
let merge a1 a2 = 
  match a1, a2 with
  | ((Sure _) as a), _ 
  | _, ((Sure _) as a)
  | NotFound, a
  | a, NotFound ->
      a

  | Unsure (q1, v1), Unsure (q2, v2) ->
      if v1 = v2 then
        a1
      else if q1 > q2 then
        a1
      else
        a2

let value = 
  function 
    | Unsure (_, a) | Sure a -> Some a
    | NotFound -> None

let is_sure = 
  function 
    | Sure _ -> true
    | Unsure _ | NotFound -> false

(** Try to determine order, using guessed answer for 
    package and version
 *)
let order ~ctxt a_pkg a_ver =
  match value a_pkg, value a_ver with 
  | Some pkg_s, Some ver ->
      begin
        catch 
          (fun () ->
            ODBStorage.PkgVer.elements pkg_s
            >>= fun lst ->
            (* Try to insert this version between two. If version = latest -> sure
             * otherwise -> unsure.
             *)
            let cmp =
              OASISVersion.version_compare 
            in
            let rec find_position =
              function
                | v1 :: ((v2 :: _) as tl) ->
                    let ver1 = v1.ODBPkgVer.ver in
                    let ver2 = v2.ODBPkgVer.ver in

                    if cmp ver1 ver = 0 || cmp ver2 ver = 0 then
                      begin
                        error ~ctxt 
                        (f_ "Version '%s' for package '%s' already exists")
                        (OASISVersion.string_of_version ver) pkg_s
                        >>= fun () ->
                        return NotFound
                      end
                    else if cmp ver ver1 < 0 then
                      begin
                        let order = 
                          v1.ODBPkgVer.ord - 10
                        in
                          return (Unsure (0.5, order))
                      end
                    else if cmp ver1 ver < 0 && cmp ver ver2 < 0 then
                      begin
                        let order = 
                          (v1.ODBPkgVer.ord + v2.ODBPkgVer.ord) / 2
                        in
                          return (Unsure (0.5, order))
                      end
                    else
                      begin
                        find_position tl
                      end

                | v1 :: [] ->
                    begin
                      return (Sure (v1.ODBPkgVer.ord + 10))
                    end

                | [] ->
                    begin
                      return NotFound
                    end
            in
              find_position lst)

          (function 
            | Not_found ->
                (* The package doesn't exist, use default order. *)
                return (Sure 0)
            | e ->
                fail e)
      end

  | _, _ ->
      return NotFound


(** Try to guess parameter for a tarball
 *)
let run ~ctxt fn an dn =
  let completions = 
    [
      "tarball_name", tarball_name;
      "topdir_name",  topdir_name;
      "configure_ac", configure_ac;
      "meta",         meta;
    ]
  in

  let catch_log (nm, run) dflt = 
    debug ~ctxt
      (f_ "Running completion '%s' on '%s'")
      nm fn 
    >>= fun () ->
      catch 
        (fun () ->
          run ~ctxt fn an dn)
        (fun e ->
          warning ~ctxt
            (f_ "Error in completion '%s' on '%s': %s")
            nm fn (ODBMessage.string_of_exception e)
          >>= fun () ->
          return dflt)
  in

  let debug_value pkg ver =
    let string_of_answer =
      function
        | Sure v -> 
            v 
        | Unsure (q, v) -> 
            Printf.sprintf "%s with %.2f%%" v (q *. 100.)
        | NotFound ->
            s_ "unknown"
    in
      debug ~ctxt
        (f_ "Package: %s")
        (string_of_answer pkg)
      >>= fun () ->
      debug ~ctxt
        (f_ "Version: %s")
        (string_of_answer ver)
  in

    debug ~ctxt (f_ "Looking for _oasis file in '%s'") fn
    >>= fun () ->
    (* Completion with _oasis is top priority because we can have
     * a definitive answer with this method
     *)
    catch_log
      ("oasis", oasis)
      (None, NotFound, NotFound)
    >>= fun (oasis_content, pkg, ver) ->
    begin
      let further_guesses = 
        match pkg, ver with
        | Sure _, Sure _ ->
            (* We have everything no need to continue *)
            debug_value pkg ver
            >>= fun () ->
            debug ~ctxt (f_ "Found an _oasis file, no need to continue")
            >>= fun () ->
            return (pkg, ver) 

        | _, _ ->
            (* Apply further completion methods *)
            Lwt_list.fold_left_s
              (fun (pre_pkg, pre_ver) completion ->
                catch_log completion (NotFound, NotFound)
                >>= fun (cur_pkg, cur_ver) ->
                begin
                  debug_value cur_pkg cur_ver
                  >>= fun () ->
                  return 
                    (merge pre_pkg cur_pkg,
                    merge pre_ver cur_ver)
                end)
              (pkg, ver)
              completions
      in
        further_guesses
        >>= fun (a_pkg, a_ver_s) ->
        let a_ver =
          (* Use a version rather than a string *)
          let vos  = OASISVersion.version_of_string in
          match a_ver_s with 
          | Sure s -> Sure (vos s)
          | Unsure (q, s) -> Unsure (q, vos s)
          | NotFound -> NotFound
        in
          order ~ctxt a_pkg a_ver
          >>= fun a_ord ->
          return 
            {
              ct_pkg    = a_pkg;
              ct_ver    = a_ver;
              ct_ord    = a_ord;
              ct_oasis  = oasis_content;
            }
    end

(* TODO: Other possible checks:
   - if user <> www-data then assert user provided = user
 *)
