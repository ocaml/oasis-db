
open ODBProcess
open ODBGettext
open ODBContext
open Lwt

exception NoHandler of string

let () = 
  Printexc.register_printer
    (function 
       | NoHandler fn ->
           Some 
             (Printf.sprintf 
                (f_ "Don't know how to uncompress '%s'")
                fn)
       | e ->
           None)

type filename = string

let all = ref []

let register_handler suff t =
  all := (suff, t) :: !all;
  t

let handler_prog fprog fargs ctxt ?src chn dn =
  let prog = fprog ctxt in
  let args = fargs ctxt dn in
    (* Uncompress the tarball *)
    (match src with 
       | Some fn ->
           ODBMessage.info ~ctxt (f_ "Uncompressing tarball %s") fn
       | None ->
           return ())
    >>= fun () ->
    run_logged ~ctxt ~stdin_chn:chn prog args

let register_handler_prog suff fprog fargs =
  register_handler suff (handler_prog fprog fargs)

let tgz = 
  register_handler_prog ".tgz"
    (fun ctxt -> ctxt.tar) 
    (fun ctxt dn -> ["-C"; dn; "-xz"])

let tgz' = 
  register_handler ".tar.gz" tgz

let tbz =
  register_handler_prog ".tar.bz2"
    (fun ctxt -> ctxt.tar)
    (fun ctxt dn -> ["-C"; dn; "-xj"])

let tbz' = 
  register_handler ".tbz" tbz

let zip =
  let handler ctxt ?src chn dn = 
    let tarball_fn = 
      Filename.temp_file "oasis-db-archive-" ".zip"
    in
      finalize 
        (fun () ->
           Lwt_io.with_file ~mode:Lwt_io.output tarball_fn
             (fun chn_out ->
                Lwt_io.write_chars chn_out (Lwt_io.read_chars chn))
           >>= fun () ->
           run_logged ~ctxt ctxt.unzip ["-d"; dn; tarball_fn])
        (fun () ->
           return (Sys.remove tarball_fn))
  in
    register_handler ".zip" handler

let of_filename fn =
  let rec find_handler =
    function 
      | (suf, hdl) :: tl ->
          if Filename.check_suffix fn suf then
            hdl
          else
            find_handler tl

      | [] ->
          raise (NoHandler fn)
  in
    find_handler !all

let uncompress ~ctxt ?src hdl chn dn =
  hdl ctxt ?src chn dn 

let uncompress_tmp_dir ~ctxt fd nm f = 
  FileUtilExt.with_temp_dir "oasis-db-" "" 
    (fun dn ->
       uncompress ~ctxt fd nm dn
       >>= fun an ->
       (* Do something with content *)
       f nm an dn)

let chop_suffix hdl fn =
  let rec find =
    function
      | (suff, hdl') :: tl ->
          if hdl' == hdl then
            if Filename.check_suffix fn suff then
              String.sub fn 0 ((String.length fn) - (String.length fn))
            else
              fn
          else
            find tl

      | [] ->
          raise (NoHandler fn)
  in
    find !all
