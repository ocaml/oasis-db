
(** Tests for git library 
   
    @author Sylvain Le Gall
  *)

open OUnit 
open TestCommon
open Lwt

let tests =
  "Git" >::
  bracket_tmpdir
    (fun dir ->
       let t = 
         Git.create ~work_tree:dir ()
       in
       let job =
         Git.init t
         >>= fun () ->
         begin
           let fn = 
             Filename.concat dir "foo.txt"
           in
             FileUtil.touch fn;
             Git.add fn t
         end
         >>= fun () ->
         Git.commit "Initial commit" t
       in
         Lwt_main.run job)
