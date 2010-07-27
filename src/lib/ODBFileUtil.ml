
open Lwt
open Unix
open Lwt_unix
open ODBGettext

type filename = string

type iter_t = 
  | File    of filename
  | PreDir  of filename (* Beware that the dir is opened *)
  | PostDir of filename (* Dir is closed *)

exception ENotDir of filename
exception ENotExist of filename

let fold_dir f fn a = 
  let rec fold_dir_aux fd a = 
    catch 
      (fun () ->
         readdir fd 
         >>= fun bn ->
           if bn = Filename.current_dir_name || 
              bn = Filename.parent_dir_name then
             begin
               fold_dir_aux fd a
             end
           else
             begin
               let fn = 
                 Filename.concat fn bn 
               in
                 f fn bn a
                 >>=
                 fold_dir_aux fd
             end)

      (function 
         | End_of_file ->
             (* End listing a directory *)
             return a
         | e ->
             fail e) 

  in
    catch 
      (fun () ->
         let fd = 
           opendir fn
         in
           finalize 
             (fun () ->
               fold_dir_aux fd a)

             (fun () ->
               (* Always close dir *)
               return (closedir fd)))

      (function 
         | (Unix_error (e, _, _)) as exc -> 
             if e = ENOTDIR then
               fail (ENotDir fn)
             else if e = ENOENT then
               fail (ENotExist fn)
             else
               fail exc

         | exc ->
             fail exc)


let fold_fs f fn a = 
  let rec fold_aux f fn a = 
    catch 
      (fun () ->
        let do_predir (first, a) = 
          if first then 
            f (PreDir fn) a
          else
            return a
        in

        (* Recurse inside fold_dir *)
        let f_fold sub_fn _ (first, a) = 
          do_predir (first, a)
          >>= fun a ->
          fold_aux f sub_fn a
          >>= fun a ->
          return (false, a)
        in

           fold_dir f_fold fn (true, a)
           >>= 
           (* If the directory is empty, we don't yet 
              have called PreDir.
            *)
           do_predir 
           >>= fun a ->
           f (PostDir fn) a)

      (function 
         | ENotDir fn ->
             (* File handling *)
             f (File fn) a

         | ENotExist _ ->
             (* No file, no dir, just skip *)
             return a

         | exc ->
             fail exc)

  in
    fold_aux f fn a
  

let iter_fs f fn = 
  fold_fs (fun fn () -> f fn) fn ()


exception RmDirNoRecurse of string

let rm ?section ?logger ?(recurse=false) lst = 
  Lwt_list.iter_p
    (iter_fs 
       (function
          | PreDir fn ->
              begin
                if recurse then
                  return ()
                else
                  fail (RmDirNoRecurse fn)
              end

          | PostDir fn ->
              begin
                try 
                  Lwt_log.debug_f
                    ?section ?logger
                    (f_ "Remove directory '%s'")
                    fn
                  >>= fun () -> 
                  return (Unix.rmdir fn)
                with e ->
                  fail e 
              end

          | File fn ->
              begin
                try 
                  Lwt_log.debug_f
                    ?section ?logger
                    (f_ "Remove file '%s'")
                    fn
                  >>= fun () ->
                  return (Unix.unlink fn)
                with e ->
                  fail e
              end))
    lst

let mkdir dn perm = 
  try 
    Unix.mkdir dn perm;
    return ()
  with e ->
    fail e 

let temp_dir pre suf = 
  let rec temp_dir_aux n = 
    let dn =
      Printf.sprintf "%s%06d%s" pre n suf
    in
      catch 
        (fun () -> 
           (mkdir dn 0o700)
           >>= fun () -> return dn)
        (function
           | Unix.Unix_error (e, _, _) when e = Unix.EEXIST ->
              temp_dir_aux (n + 1)
           | e ->
               fail e)
  in
  let start = 
    (int_of_float ((Unix.gettimeofday ()) *. 1000.))
      mod 
    1000000
  in
    temp_dir_aux start

