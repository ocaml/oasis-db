
open ODBProcess
open ODBGettext
open ODBContext
open Lwt

exception NoHandler of string

let string_of_exception =
  function
    | NoHandler fn ->
        Printf.sprintf 
          (f_ "Don't know how to uncompress '%s'")
          fn

    | e ->
        raise e

let uncompress ~ctxt fd nm dn = 
  let run prg args = 
    run_logged ~ctxt prg args
  in
  let tarball_fn = 
    Filename.concat dn nm
  in

  let handlers = 
    [ 
      ".tgz",
      ctxt.tar,
      ["-C"; dn; "-xzf"; tarball_fn];

      ".tar.gz",
      ctxt.tar,
      ["-C"; dn; "-xzf"; tarball_fn];

      ".tar.bz2",
      ctxt.tar,
      ["-C"; dn; "-xjf"; tarball_fn];

      ".zip",
      ctxt.unzip,
      ["-d"; dn; tarball_fn];
    ]
  in

  let rec find_handler =
    function 
      | (suf, prg, args) :: tl ->
          if Filename.check_suffix nm suf then
            run prg args
            >>= fun () -> 
            return (Filename.basename (Filename.chop_suffix nm suf))
          else
            find_handler tl

      | [] ->
          fail (NoHandler nm)
  in
    (* Copy the tarball to temporary directory *)
    LwtExt.IO.copy_fd fd tarball_fn 
    >>= fun () ->
    (* Uncompress the tarball *)
    find_handler handlers 
    >>= fun res ->
    FileUtilExt.rm [tarball_fn]
    >|= fun _ ->
    res

let uncompress_tmp_dir ~ctxt fd nm f = 
  FileUtilExt.with_temp_dir "oasis-db-" "" 
    (fun dn ->
       uncompress ~ctxt fd nm dn
       >>= fun an ->
       (* Do something with content *)
       f nm an dn)
