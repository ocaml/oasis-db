
open ODBMessage
open ODBGettext
open Lwt
open Sexplib

exception FileTooBig of int64 * string

module IO =
struct 

  let with_file_content fn =
    Lwt_io.with_file
      ~mode:Lwt_io.input fn
      (fun chn ->
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
        end)

  let digest fn = 
    (* TODO: big file *)
    with_file_content fn 
    >>= fun str ->
    return (Digest.string str)

  let sexp_load ~ctxt vt_of_sexp upgrade fn =
    with_file_content fn 
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

  let sexp_dump ~ctxt sexp_of_vt version fn t = 
    let str = 
      Sexp.to_string_hum ~indent:2 (sexp_of_vt (version t))
    in
      debug ~ctxt
        (f_ "Creating file '%s'")
        fn
      >>= fun () ->
      Lwt_io.with_file
        ~mode:Lwt_io.output fn
        (fun chn ->
          Lwt_io.write_from_exactly
            chn str 0 (String.length str))

end
