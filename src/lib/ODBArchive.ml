
open ODBProcess
open ODBGettext
open ODBFileUtil
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
    run_logged ~ctxt ~stdin_fd:fd prg args
  in
  let handlers = 
    [ 
      ".tgz",
      ctxt.tar,
      ["-C"; dn; "-xz"];

      ".tar.gz",
      ctxt.tar,
      ["-C"; dn; "-xz"];

      ".tar.bz2",
      ctxt.tar,
      ["-C"; dn; "-xjf"];

      ".zip",
      ctxt.unzip,
      ["-d"; dn];
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

    find_handler handlers 


let uncompress_tmp_dir ~ctxt fd nm f = 
  with_temp_dir ~ctxt "oasis-db-" "" 
    (fun dn ->
       uncompress ~ctxt fd nm dn
       >>= fun an ->
       (* Do something with content *)
       f nm an dn)
