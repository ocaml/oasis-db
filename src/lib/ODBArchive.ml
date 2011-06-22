
open ODBProcess
open ODBGettext
open ODBContext
open Lwt

exception NoHandler of string
exception NoCompress of string

let () = 
  Printexc.register_printer
    (function 
       | NoHandler fn ->
           Some 
             (Printf.sprintf 
                (f_ "Don't know how to uncompress '%s'")
                fn)
       | NoCompress fn ->
           Some 
             (Printf.sprintf
                (f_ "No method to compress to '%s'")
                fn)
       | e ->
           None)

type filename = string

type t =
    {
      arch_uncompress: ?src:filename -> 
        ODBContext.t -> Lwt_io.input_channel -> filename -> unit Lwt.t;

      arch_compress: ?tgt:filename ->
        ODBContext.t -> filename -> Lwt_io.output_channel -> unit Lwt.t;
    }

let all = ref []

let register_handler suff t =
  all := (suff, t) :: !all;
  t

let handler_prog_uncompress fprog fargs ?src ctxt chn dn =
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

let handler_prog_compress fprog fargs ?tgt ctxt dn chn =
  let prog = fprog ctxt in
  let args = fargs ctxt dn in
    (* Uncompress the tarball *)
    (match tgt with 
       | Some fn ->
           ODBMessage.info ~ctxt (f_ "Compressing tarball %s") fn
       | None ->
           return ())
    >>= fun () ->
    run_logged ~ctxt ~stdout_chn:chn prog args

let register_handler_prog suff fprog fargs1 fargs2 =
  register_handler suff 
    {
      arch_uncompress = handler_prog_uncompress fprog fargs1;
      arch_compress   = handler_prog_compress fprog fargs2;
    }

let tgz = 
  register_handler_prog ".tgz"
    (fun ctxt -> ctxt.tar) 
    (fun ctxt dn -> ["-C"; dn; "-xz"])
    (fun ctxt dn -> ["-C"; FilePath.dirname dn; "-cz"; FilePath.basename dn])

let tgz' = 
  register_handler ".tar.gz" tgz

let tbz =
  register_handler_prog ".tar.bz2"
    (fun ctxt -> ctxt.tar)
    (fun ctxt dn -> ["-C"; dn; "-xj"])
    (fun ctxt dn -> ["-C"; FilePath.dirname dn; "-cj"; FilePath.basename dn])

let tbz' = 
  register_handler ".tbz" tbz

let zip =
  let handler_uncompress ?src ctxt chn dn = 
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
    register_handler ".zip" 
      {
        arch_compress = 
          (fun ?tgt _ _ _ -> 
             match tgt with 
               | Some fn -> 
                   fail (NoCompress fn)
               | None ->
                   fail (NoCompress "filename.zip"));

        arch_uncompress = handler_uncompress;
      }

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
  hdl.arch_uncompress ctxt ?src chn dn 

let compress ~ctxt ?tgt hdl chn dn = 
  hdl.arch_compress ctxt ?tgt chn dn 

let uncompress_tmp_dir ~ctxt fd nm f = 
  FileUtilExt.with_temp_dir "oasis-db-" "" 
    (fun dn ->
       uncompress ~ctxt fd nm dn
       >>= fun an ->
       (* Do something with content *)
       f nm an dn)

let chop_suffix hdl fn =
  try 
    let suff, _ = 
      List.find 
        (fun (_, hdl') -> hdl' == hdl)
        !all
    in
      if Filename.check_suffix fn suff then
        String.sub fn 0 ((String.length fn) - (String.length fn))
      else
        fn
  with Not_found ->
    raise (NoHandler fn)

let add_suffix hdl fn =
  try
    let suff, _ = 
      List.find
        (fun (_, hdl') -> hdl' == hdl)
        !all
    in
      fn^suff
  with Not_found ->
    raise (NoHandler fn)
