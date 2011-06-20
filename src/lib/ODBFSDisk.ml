
(** Filesystem that is based on a physical fs

    @author Sylvain Le Gall
  *)

open Lwt

exception Not_subdir of (ODBVFS.filename * ODBVFS.filename)

let () = 
  Printexc.register_printer
    (function
       | Not_subdir (root, fn) ->
           Some 
             (Printf.sprintf
                "Filename '%s' is not a subdirectory of '%s'"
                fn root)
       | _ -> 
           None)

class read_only root = 
object (self) 

  inherit ODBVFS.read_only

  val root = 
    if FilePath.is_relative root then
      FilePath.concat (FileUtil.pwd ()) root
    else
      root

  method id = 
    "file://"^root

  (** Make the file absolute to root *)
  method private rebase fn =
    let abs_fn = 
      FilePath.make_absolute root fn
    in
      if FilePath.compare root abs_fn = 0 || 
         FilePath.is_subdir abs_fn root then
        abs_fn
      else
        raise (Not_subdir (root, abs_fn))

  method private rebase_lwt fn =
    try 
      return (self#rebase fn)
    with e ->
      fail e

  (** Make the file relative to root *)
  method private unbase fn =
    FilePath.make_relative root fn

  (** Test file existence *)
  method file_exists fn =
    self#rebase_lwt fn >|= Sys.file_exists

  (** Test if it is a directory *)
  method is_directory fn =
    self#rebase_lwt fn >|= Sys.is_directory

  (** Open a file for reading *)
  method open_in_low fn =
    self#rebase_lwt fn
    >>= fun fn' ->
    Lwt_io.open_file ~mode:Lwt_io.input fn'
    >|= fun chn ->
    ((fun () -> return ()), chn)

  (** Return Unix stat of a file *)
  method stat fn =
    try 
      return (Unix.LargeFile.stat (self#rebase fn))
    with e ->
      fail e

  (** Read a directory content *)
  method readdir dn =
     try
       return (Sys.readdir (self#rebase dn))
     with e ->
       fail e

  (** Return the name of the filename on disk (absolute). *)
  method real_filename fn = 
    self#rebase fn

end

(** Read-write filesystem *)
class read_write root = 
object (self) 

  inherit ODBVFS.read_write
  inherit read_only root

  (** Open a file for writing *)
  method open_out_low ?flags fn =
    self#rebase_lwt fn
    >>= fun fn' ->
    Lwt_io.open_file ?flags ~mode:Lwt_io.output fn'
    >|= fun chn ->
    (fun () -> return ()), chn

  (** Create a directory with given permissions *)
  method mkdir_low dn perm =
    self#rebase_lwt dn 
    >>= fun fn' ->
    FileUtilExt.mkdir fn' perm


  method rmdir fn =
    return (Unix.rmdir (self#rebase fn))

  method unlink fn =
    return (Unix.unlink (self#rebase fn))

end
