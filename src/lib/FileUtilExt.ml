
open Lwt
open Unix
open Lwt_unix
open Lwt_io
open ODBGettext

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


let fold f fn a = 
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
  

let iter f fn = 
  fold (fun fn () -> f fn) fn ()

exception RmDirNoRecurse of string

let rm ?(recurse=false) lst = 
  Lwt_list.fold_left_s
    (fun acc fn ->
       if Sys.file_exists fn then
         fold
           (fun fnt acc ->
              match fnt with
                | PreDir fn ->
                    begin
                      if recurse then
                        return acc 
                      else
                        fail (RmDirNoRecurse fn)
                    end

                | PostDir fn ->
                    begin
                      try 
                        Unix.rmdir fn;
                        return (fn :: acc)
                      with e ->
                        fail e 
                    end

                | File fn 
                | Symlink fn ->
                    begin
                      try 
                        Unix.unlink fn;
                        return (fn :: acc)
                      with e ->
                        fail e
                    end)
           fn acc
       else
         return acc)
    []
    lst
    >|= fun lst ->
    List.rev lst

let mkdir ?(ignore_exist=false) dn perm = 
  try 
    Unix.mkdir dn perm;
    return ()
  with 
  | Unix.Unix_error (e, _, _) when e = Unix.EEXIST && ignore_exist ->
      return ()
  | e ->
      fail e 

let temp_dir pre suf = 
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

let with_temp_dir pre suf f = 
  temp_dir pre suf 
  >>= fun dn ->
  (finalize
     (fun () -> 
        f dn)
     (fun () ->
        rm ~recurse:true [dn]
        >|= 
        ignore))

let cp lst tgt =
  let cp_one src tgt =
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
      Lwt_list.map_s
        (fun src ->
           let tgt =
             Filename.concat tgt (Filename.basename src)
           in
             cp_one src tgt
             >>= fun () ->
             return (src, tgt))
        lst
    else
      match lst with 
        | [src] ->
            cp_one src tgt
            >>= fun () ->
            return [src, tgt]
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


let mv src tgt =
  let pwd = FileUtil.pwd () in
  let rec mv' acc fln_src fln_dst =
    let acc_del_src, acc_del_tgt, acc_mv = acc in
    let fln_src_abs =  FilePath.make_absolute pwd fln_src in
    let fln_dst_abs =  FilePath.make_absolute pwd fln_dst in
      if compare fln_src_abs fln_dst_abs <> 0 then
        begin
          if Sys.is_directory fln_dst_abs then
            begin
              mv' 
                acc
                fln_src_abs
                (FilePath.make_absolute 
                   fln_dst_abs 
                   (FilePath.basename fln_src_abs))
            end

          else if Sys.file_exists fln_dst_abs then
            begin
              rm [fln_dst_abs]
              >>= fun del_lst ->
              mv' 
                (acc_del_src, del_lst @ acc_del_tgt, acc_mv) 
                fln_src_abs fln_dst_abs
            end

          else if Sys.file_exists fln_src_abs then
            begin
              let acc_mv = (fln_src_abs, fln_dst_abs) :: acc_mv in 
                try 
                  begin
                    Sys.rename fln_src_abs fln_dst_abs;
                    return (acc_del_src, acc_del_tgt, acc_mv) 
                  end

                with Sys_error _ ->
                  begin
                    cp [fln_src_abs] fln_dst_abs
                    >>= fun (_lst : (filename * filename) list) ->
                    rm ~recurse:true [fln_src_abs]
                    >>= fun del_lst ->
                    return (del_lst @ acc_del_src, acc_del_tgt, acc_mv)
                  end
            end
          else
            begin
              fail FileUtil.MvNoSourceFile
            end
        end
      else
        return acc
  in
    mv' ([], [], []) src tgt
;;
