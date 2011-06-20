
(** Apply file locking operations on a VFS
   
    @author Sylvain Le Gall
  *)


open Lwt

class read_only fs rwlock =
object (self)
  inherit ODBVFS.read_only

  method id = fs#id

  (** Acquire read resources *)
  method private read_begin = 
    ODBRWLock.read_lock rwlock

  (** Release read resources *)
  method private read_end =
    ODBRWLock.read_unlock rwlock

  (** Acquire and release resources *)
  method private do_read: 'a. (unit -> 'a Lwt.t) -> 'a Lwt.t  =
    fun f ->
      self#read_begin 
      >>= 
      f 
      >>= fun res -> 
      self#read_end 
      >|= fun () ->
      res

  method file_exists fn = 
    self#do_read (fun () -> fs#file_exists fn)

  method is_directory fn =
    self#do_read (fun () -> fs#is_directory fn)

  method open_in_low fn =
    self#read_begin 
    >>= fun () -> 
    fs#open_in fn
    >|= fun (close_chn, chn) ->
    (fun () -> 
       close_chn () 
       >>= fun () ->
       self#read_end),
    chn

  method stat fn = 
    self#do_read (fun () -> fs#stat fn)

  method readdir fn = 
    self#do_read (fun () -> fs#readdir fn)
end

class read_write fs rwlock =
object (self)
  inherit read_only fs rwlock
  inherit ODBVFS.read_write

  initializer 
    begin
      (* Relay events from fs *)
      let _i : int = 
        fs#watch_add self#watch_notify 
      in
        ()
    end

  method private write_begin =
    ODBRWLock.write_lock rwlock

  method private write_end =
    ODBRWLock.write_unlock rwlock

  (** Acquire and release resources *)
  method private do_write: 'a. (unit -> 'a Lwt.t) -> 'a Lwt.t  =
    fun f ->
      self#write_begin 
      >>= 
      f 
      >>= fun res -> 
      self#write_end 
      >|= fun () ->
      res

  method open_out_low ?flags fn =
    self#write_begin 
    >>= fun () -> 
    fs#open_out_low ?flags fn
    >|= fun (close_fun, chn) ->
    let close_fun' () =
      close_fun ()
      >>= fun () ->
      self#write_end
    in
      close_fun', chn


  method mkdir_low fn perm =
    self#do_write (fun () -> fs#mkdir_low fn perm)

  method unlink fn =
    self#do_write (fun () -> fs#unlink fn)

  method rmdir fn =
    self#do_write (fun () -> fs#rmdir fn)

  method cp other_fs lst tgt =
    (* TODO: prevent read lock other_fs if other_fs = fs ? *)
    self#do_write (fun () -> fs#cp other_fs lst tgt)
end

