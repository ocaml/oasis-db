
open ODBMessage
open ODBGettext
open Lwt
open Sexplib

exception FileTooBig of int64 * string

module IO =
struct 

  let with_file_content_chn ~fn chn =
    Lwt_io.length chn
    >>= fun ln64 ->
    begin
      if ln64 < Int64.of_int Sys.max_string_length then
        begin
          let str = 
            String.make (Int64.to_int ln64) 'X'
          in
            Lwt_io.read_into_exactly chn str 0 (Int64.to_int ln64)
            >>= fun () ->
            return str
        end
      else
        begin
          fail (FileTooBig (ln64, fn))
        end
    end

  let with_file_content fn =
    Lwt_io.with_file
      ~mode:Lwt_io.input fn
      (with_file_content_chn ~fn)

  let digest_chn ~fn chn =
    (* TODO: big file *)
    with_file_content_chn ~fn chn
    >>= fun str ->
    return (Digest.string str)

  let digest fn = 
    (* TODO: big file *)
    with_file_content fn 
    >>= fun str ->
    return (Digest.string str)

  let sexp_load_chn ~ctxt ~fn vt_of_sexp upgrade chn =
    with_file_content_chn ~fn chn
    >>= fun str ->
    try 
      match Sexp.parse str with 
        | Sexp.Done (res, _) ->
            upgrade ~ctxt (vt_of_sexp res)
        | Sexp.Cont _ ->
            fail 
              (Failure 
                (Printf.sprintf 
                  (f_ "Incomplete S-Expression file '%s'")
                  fn))
     with e ->
       fail e

  let sexp_load ~ctxt vt_of_sexp upgrade fn =
    Lwt_io.with_file
      ~mode:Lwt_io.input fn
      (sexp_load_chn ~ctxt ~fn vt_of_sexp upgrade)

  let sexp_dump_chn ~ctxt ~fn sexp_of_vt version t chn = 
    let str = 
      Sexp.to_string_hum ~indent:2 (sexp_of_vt (version t))
    in
      debug ~ctxt
        (f_ "Creating file '%s'")
        fn
      >>= fun () ->
      Lwt_io.write_from_exactly
        chn str 0 (String.length str)

  let sexp_dump ~ctxt sexp_of_vt version fn t = 
    Lwt_io.with_file
      ~mode:Lwt_io.output fn
      (sexp_dump_chn ~ctxt ~fn sexp_of_vt version t)

  let copy_fd fd fn = 
    try 
      let fd = Unix.dup fd
      in
      let _i : int = 
        Unix.lseek fd 0 Unix.SEEK_SET
      in
      let chn_in = 
        Lwt_io.of_unix_fd ~mode:Lwt_io.input (Unix.dup fd)
      in
        finalize 
          (fun () ->
             Lwt_io.with_file
               ~mode:Lwt_io.output
               fn
               (fun chn_out ->
                  let buf = 
                    String.make 4096 'X'
                  in
                  let rec copy acc = 
                    Lwt_io.read_into chn_in buf 0 (String.length buf)
                    >>= fun read ->
                    if read > 0 then
                      begin
                        Lwt_io.write_from_exactly chn_out buf 0 read 
                        >>= fun () ->
                        copy (read + acc)
                      end
                    else
                      begin
                        return ()
                      end
                  in
                    copy 0))
          (fun () ->
             Unix.close fd;
             Lwt_io.close chn_in)
    with e ->
      fail e
end
