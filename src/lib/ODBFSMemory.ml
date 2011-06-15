
(** Filesystem that keeps its data in memory
  *)

open Lwt
(* TODO: map ODBFSTree exceptions to Unix ones *)
  (* map: 
   * open bad file Unix.Unix_error (Unix.ENOENT, "open", "")
   * open bad dir Unix.Unix_error (Unix.ENOENT, "opendir", "")
   *)


class read_only fstree =
object (self)

  inherit ODBVFS.read_only 

  method id =
    "memory://"

  method file_exists fn = 
    return (ODBFSTree.mem fstree fn)

  method is_directory fn =
    return (ODBFSTree.is_dir fstree fn)

  method open_in_low fn = 
    let str = 
      ODBFSTree.find_file fstree fn 
    in
      return 
        ((fun () -> return ()), 
         Lwt_io.of_string ~mode:Lwt_io.input str)

  method stat fn = 
    let now = 
      Unix.gettimeofday ()
    in
    let default = 
      {Unix.LargeFile.
        st_dev   = 0;
        st_ino   = 0;
        st_kind  = Unix.S_REG;
        st_perm  = 0o644;
        st_nlink = 0;
        st_uid   = 0;
        st_gid   = 0;
        st_rdev  = 0;
        st_size  = 0L;
        st_atime = now;
        st_mtime = now;
        st_ctime = now;
      } 
    in
    let st = 
      match ODBFSTree.find fstree fn with 
        | ODBFSTree.File str ->
            {default with 
                 Unix.LargeFile.
                 st_kind = Unix.S_REG;
                 st_perm = 0o644}

        | ODBFSTree.Dir _ ->
            {default with 
                 Unix.LargeFile.
                 st_kind = Unix.S_DIR;
                 st_perm = 0o755}
    in
      return st

  method readdir fn =
    return (Array.of_list (ODBFSTree.elements fstree fn))

end

class read_write fstree =
object (self)

  inherit ODBVFS.read_write
  inherit read_only fstree

  method open_out_low 
         ?(flags=[Unix.O_WRONLY; 
                  Unix.O_CREAT; 
                  Unix.O_TRUNC; 
                  Unix.O_NONBLOCK])
         fn = 
    try
      let has_flag flg = List.mem flg flags in
      let has_append = has_flag Unix.O_APPEND in
      let has_creat  = has_flag Unix.O_CREAT in
      let has_trunc  = has_flag Unix.O_TRUNC in

      let mkchn str =
        let str = 
          if has_trunc && str <> "" && ODBFSTree.mem fstree fn then
            begin
              ODBFSTree.add_file fstree fn "";
              ""
            end
          else 
            begin
              str 
            end
        in
        let t, chn = 
          LwtExt.IO.MemoryOut.open_out str
        in
        let close () = 
          try
            let str = LwtExt.IO.MemoryOut.to_string t in
              ODBFSTree.add_file fstree fn str;
              return ()
          with e -> 
            fail e
        in
          begin
            if has_append then 
              begin
                Lwt_io.length chn 
                >>= fun pos ->
                Lwt_io.set_position chn pos
              end
            else
              begin
                return ()
              end
          end
          >>= fun () ->
          return (close, chn)
      in

      let not_found =
        if has_creat then
          Some (fun _ _ -> mkchn "")
        else 
          None
      in
        ODBFSTree.find_low
          ?not_found
          fstree
          fn
          (fun _ ->
             function
               | ODBFSTree.Dir _ ->
                   raise 
                     (Unix.Unix_error
                        (Unix.ENOENT, "open", ""))
               | ODBFSTree.File str ->
                   mkchn str)

    with e ->
      fail e 


  method mkdir ?(ignore_exist=false) fn perm = 
    try 
      if ODBFSTree.mem fstree fn && 
         ODBFSTree.is_dir fstree fn && 
         ignore_exist then
        return ()
      else
        return (ODBFSTree.add_dir fstree fn ())
    with e ->
      fail e

  method rmdir fn = 
    try 
      if ODBFSTree.elements fstree fn = [] then
        return (ODBFSTree.remove fstree fn)
      else
        fail (Unix.Unix_error (Unix.EINVAL, "rmdir", fn))
    with e ->
      fail e

  method unlink fn = 
    try 
      if ODBFSTree.mem fstree fn &&
         not (ODBFSTree.is_dir fstree fn) then
        return (ODBFSTree.remove fstree fn)
      else
        fail (Unix.Unix_error (Unix.EISDIR, "unlink", fn))
    with e ->
      fail e

end

let cp_directories ?ignore_exist fs_src fs_tgt =
  ODBVFS.fold 
    (fun ev () ->
       match ev with
         | `PreDir fn ->
             if fn <> "" then 
               begin
                 fs_src#stat fn 
                 >>= fun st ->
                 fs_tgt#mkdir ?ignore_exist fn st.Unix.LargeFile.st_perm
               end
             else
               begin
                 return ()
               end
         | `PostDir _ | `File _ ->
             return ())
    fs_src
    ""
    ()
