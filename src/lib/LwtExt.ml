
open ODBMessage
open ODBGettext
open Lwt
open Sexplib

exception FileTooBig of int64 

module IO =
struct 

  module MemoryOut = 
  struct
    (* In memory file *)
    type t = (int64 * Lwt_bytes.t) ref 

    let open_out ?buffer_size str = 
      let t = 
        ref (0L, Lwt_bytes.of_string str) 
      in

      let seek i64 sk_cmd = 
        let (cur_pos, str) = !t in
        let pos64 = 
          match sk_cmd with
            | Unix.SEEK_SET -> 
                i64
            | Unix.SEEK_CUR ->
                Int64.add cur_pos i64
            | Unix.SEEK_END ->
                Int64.sub (Int64.of_int (Lwt_bytes.length str)) i64
        in
          t := pos64, str;
          return pos64
      in

      let perform_io str start len =
        let (cur_pos64, cur_str) = !t in
        let cur_pos = Int64.to_int cur_pos64 in
        let str' = 
          let strlen = cur_pos + len in
            if strlen <= Lwt_bytes.length cur_str then
              begin
                cur_str
              end
            else
              begin
                let res = 
                  Lwt_bytes.create strlen
                in
                  Lwt_bytes.blit cur_str 0 res 0 (Lwt_bytes.length cur_str);
                  res
              end
        in
          Lwt_bytes.blit str start str' cur_pos len;
          t := Int64.of_int (cur_pos + len), str';
          return len
      in
        t,
        Lwt_io.make 
          ?buffer_size 
          ~seek 
          ~mode:Lwt_io.output 
          perform_io
      
    let to_string t = Lwt_bytes.to_string (snd !t)

    let with_file_out ?buffer_size f = 
      let t, chn =
        open_out ?buffer_size ""
      in
        finalize
          (fun () ->
             f chn)
          (fun () ->
             Lwt_io.close chn)
        >>= fun () ->
        return (to_string t)

  end

  module MemoryIn =
  struct
    let with_file_in str f =
      let chn =
        Lwt_io.of_string ~mode:Lwt_io.input str
      in
        finalize 
          (fun () ->
             f chn)
          (fun () ->
             Lwt_io.close chn)
  end

  let with_file_content_chn chn =
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
          fail (FileTooBig ln64)
        end
    end

  let with_file_content fn =
    Lwt_io.with_file
      ~mode:Lwt_io.input fn
      with_file_content_chn 

  let digest_chn ~fn chn =
    (* TODO: big file *)
    with_file_content_chn chn
    >>= fun str ->
    return (Digest.string str)

  let digest fn = 
    (* TODO: big file *)
    with_file_content fn 
    >>= fun str ->
    return (Digest.string str)

  let sexp_load_str ~ctxt ~fn vt_of_sexp upgrade str =
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

  let sexp_load_chn ~ctxt ~fn vt_of_sexp upgrade chn =
    with_file_content_chn chn
    >>= fun str ->
    sexp_load_str ~ctxt ~fn vt_of_sexp upgrade str

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

  let with_convert_fd ?seek ~mode fd f = 
    try 
      let fd = Unix.dup fd
      in
      let _i : int = 
        match seek with 
          | Some (pos, cmd) ->
              Unix.lseek fd pos cmd
          | None -> 
              0
      in
      let chn = 
        Lwt_io.of_unix_fd ~mode (Unix.dup fd)
      in
        finalize 
          (fun () ->
             f chn)
          (fun () ->
             Unix.close fd;
             Lwt_io.close chn)
    with e ->
      fail e

  let copy_fd_chn fd chn =
    with_convert_fd
      ~seek:(0, Unix.SEEK_SET)
      ~mode:Lwt_io.input
      fd
      (fun chn_in ->
         Lwt_io.write_chars chn (Lwt_io.read_chars chn_in))

  let copy_fd fd fn = 
     Lwt_io.with_file
       ~mode:Lwt_io.output
       fn
       (copy_fd_chn fd)
end
