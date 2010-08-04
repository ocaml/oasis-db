
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

let uncompress ~ctxt fn dn = 
  let handlers = 
    [ 
      ".tgz",
      (fun () -> run_logged ~ctxt ctxt.tar ["-C"; dn; "-xzf"; fn]);

      ".tar.gz",
      (fun () -> run_logged ~ctxt ctxt.tar ["-C"; dn; "-xzf"; fn]);

      ".tar.bz2",
      (fun () -> run_logged ~ctxt ctxt.tar ["-C"; dn; "-xjf"; fn]);

      ".zip",
      (fun () -> run_logged ~ctxt ctxt.unzip [fn; "-d"; dn]);
    ]
  in

  let rec find_handler =
    function 
      | (suf, f) :: tl ->
          if Filename.check_suffix fn suf then
            f () >>= fun () -> 
            return (Filename.basename (Filename.chop_suffix fn suf))
          else
            find_handler tl

      | [] ->
          fail (NoHandler fn)
  in

    find_handler handlers 


let uncompress_tmp_dir ~ctxt fn f = 
  temp_dir "oasis-db-" "" 
  >>= fun dn ->
  (finalize
     (fun () ->
        uncompress ~ctxt fn dn
        >>= fun an ->
        (* Do something with content *)
        f fn an dn)

     (fun () ->
        rm ~ctxt ~recurse:true [dn]))
