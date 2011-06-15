
(** Tests for LwtExt
    @author Sylvain Le Gall
  *)

open TestCommon
open OUnit
open Lwt

let tests =
  "LwtExt" >:::
  [
    "IO" >:::
    [
      "MemoryOut" >::
      (fun () ->
         let apply chn = 
           Lwt_io.write chn "abcd"
         in
         let ctnt_std =
           let fn = Filename.temp_file "oasis-db" ".txt" in
             finalize
               (fun () ->
                  Lwt_io.with_file ~mode:Lwt_io.output fn 
                    (fun chn ->
                       apply chn)
                    >>= fun () ->
                    LwtExt.IO.with_file_content fn)
               (fun () ->
                  return (Sys.remove fn))
         in
         let ctnt_mem =
           LwtExt.IO.MemoryOut.with_file_out 
             apply
         in
           assert_equal 
             ~msg:"file content"
             ~printer:(Printf.sprintf "%S")
             (Lwt_main.run ctnt_std)
             (Lwt_main.run ctnt_mem))
    ]
  ]

