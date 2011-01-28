
open Lwt
open Unix
open Lwt_unix
open Lwt_io
open ODBGettext
open ODBMessage
open ODBContext

type filename = string

type iter_t = 
  | Symlink of filename 
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
  let rec fold_aux f fn _ a = 
    try 
      match FileUtil.stat fn with 
        | {FileUtil.is_link = true} ->
            f (Symlink fn) a

        | {FileUtil.kind = FileUtil.Dir} ->
            begin
              f (PreDir fn) a
              >>= fun a ->
              fold_dir (fold_aux f) fn a
              >>= fun a ->
              f (PostDir fn) a
            end

        | _ ->
            f (File fn) a
    with 
      (* TODO: this should be handled in fileutils *)
      | FileUtil.FileDoesntExist fn ->
          begin
            try 
              let _st : stats = 
                lstat fn 
              in
                f (Symlink fn) a
            with 
              | Unix_error (e, _, _) when e = ENOENT ->
                  fail (ENotExist fn)
              | e ->
                  fail e
          end
      | e ->
          fail e
  in
    fold_aux f fn "" a
  

let iter_fs f fn = 
  fold_fs (fun fn () -> f fn) fn ()


exception RmDirNoRecurse of string

let rm ~ctxt ?(recurse=false) lst = 
  Lwt_list.iter_s
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
                debug ~ctxt (f_ "Remove directory '%s'") fn
                >>= fun () -> 
                begin
                  try 
                    Unix.rmdir fn;
                    return ()
                  with e ->
                    fail e 
                end
              end

          | File fn ->
              begin
                debug ~ctxt (f_ "Remove file '%s'") fn
                >>= fun () ->
                begin
                  try 
                    Unix.unlink fn;
                    return ()
                  with e ->
                    fail e
                end
              end

          | Symlink fn ->
              begin
                debug ~ctxt (f_ "Remove symlink '%s'") fn
                >>= fun () ->
                begin
                  try 
                    Unix.unlink fn;
                    return ();
                  with e ->
                    fail e
                end
              end))
    lst

let mkdir ?(ignore_exist=false) dn perm = 
  try 
    Unix.mkdir dn perm;
    return ()
  with 
  | Unix.Unix_error (e, _, _) when e = Unix.EEXIST && ignore_exist ->
      return ()
  | e ->
      fail e 

let temp_dir ~ctxt pre suf = 
  let rec temp_dir_aux n = 
    let dn =
      FilePath.concat 
        Filename.temp_dir_name
        (Printf.sprintf "%s%06d%s" pre n suf)
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

let with_temp_dir ~ctxt pre suf f = 
  temp_dir ~ctxt pre suf 
  >>= fun dn ->
  (finalize
     (fun () -> 
        f dn)
     (fun () ->
        rm ~ctxt ~recurse:true [dn]))

let cp ~ctxt lst tgt =
  let cp_one src tgt =
    debug ~ctxt
      (f_ "Copy file '%s' to '%s'")
      src tgt
    >>= fun () ->
    with_file ~mode:input src 
      (fun src_chn ->
        with_file ~mode:output ~perm:0o644 tgt
          (fun tgt_chn ->
            let buf =
              String.make (buffer_size src_chn) 'X'
            in
            let rec cp_aux () =
              read_into src_chn buf 0 (String.length buf) 
              >>= fun read_len ->
              if read_len > 0 then
                write_from_exactly tgt_chn buf 0 read_len
                >>= 
                cp_aux 
              else
                return ()
            in
              cp_aux ()))
  in
    if Sys.file_exists tgt && Sys.is_directory tgt then
      Lwt_list.iter_s 
        (fun src ->
           cp_one 
             src 
             (Filename.concat tgt (Filename.basename src)))
        lst
    else
      match lst with 
        | [src] ->
            cp_one src tgt
        | lst ->
            fail
              (Failure
                 (Printf.sprintf 
                    (f_ "Cannot copy the list of files %s to the file '%s'")
                    (String.concat (s_ ", ") 
                       (List.map 
                          (Printf.sprintf (f_ "'%s'"))
                          lst))
                    tgt))


let mv ~ctxt lst tgt =
  (* TODO: convert to Lwt *)
  FileUtil.mv lst tgt;
  return ()
