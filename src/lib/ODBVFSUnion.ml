

(** unionfs, use a list of VFS to create a new filesystem
   
    @author Sylvain Le Gall
  *)

open Lwt

class read_only lst = 
object (self)

  inherit ODBVFS.read_only 

  method id = 
    Printf.sprintf
      "union(%s)://" 
      (String.concat ", " (List.map (fun fs -> fs#id) lst))

  method private find_fs: 'a. string -> (#ODBVFS.read_only -> 'a Lwt.t) -> (unit -> 'a Lwt.t) -> 'a Lwt.t = 
    fun fn found not_found ->
      catch 
        (fun () ->
           Lwt_list.find_s 
             (fun fs -> fs#file_exists fn) 
             lst
           >>= fun fs ->
           found fs)
        (function
           | Not_found ->
               not_found ()
           | e -> 
               fail e)

  method file_exists fn = 
    self#find_fs 
      fn 
      (fun _ ->
         return true)
      (fun () ->
         return false)

  method is_directory fn =
    self#find_fs 
      fn 
      (fun fs ->
         fs#is_directory fn)
      (fun () -> 
         return false)

  method open_in_low fn =
    self#find_fs 
      fn
      (fun fs ->
         fs#open_in_low fn)
      (fun () ->
         fail 
           (Unix.Unix_error 
              (Unix.ENOENT, "open", fn)))

  method stat fn =
    self#find_fs 
      fn
      (fun fs ->
         fs#stat fn)
      (fun () ->
         fail 
           (Unix.Unix_error
              (Unix.ENOENT, "stat", fn)))

  method readdir fn =
    self#find_fs
      fn
      (fun fs ->
         fs#readdir fn)
      (fun () ->
         fail 
           (Unix.Unix_error
              (Unix.ENOENT, "opendir", fn)))
end

class read_write wrt lst =
object (self)

  initializer 
    begin
      let _i : int =
        wrt#watch_add self#watch_notify
      in
        ()
    end

  inherit ODBVFS.read_write 
  inherit read_only (wrt :: lst)

  method open_out_low ?flags fn =
    wrt#open_out_low ?flags fn

  method mkdir_low fn perm =
    wrt#mkdir_low fn perm

  method unlink fn =
    wrt#unlink fn

  method rmdir fn =
    wrt#rmdir fn
end
