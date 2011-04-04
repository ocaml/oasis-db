
(** Filesystem object 
    @author Sylvain Le Gall
  *)

open Lwt

type filename = string

type event =
  | FSCreated
  | FSDeleted
  | FSChanged 
  | FSMovedTo of filename 
  | FSCopiedFrom of filename

let string_of_event fn =
  let spf fmt = Printf.sprintf fmt
  in
    function
      | FSCreated ->
          spf "%s created" fn
      | FSDeleted ->
          spf "%s deleted" fn
      | FSChanged ->
          spf "%s changed" fn
      | FSMovedTo fn' ->
          spf "%s moved to %s" fn fn'
      | FSCopiedFrom fn' ->
          spf "%s copied from %s" fn fn'

exception NotSubdir of filename 

class std root = 
object (self) 

  initializer 
    begin
      (* TODO: Really lock the filesystem for our own usage (lockf?) *)
      ()
    end


  val root = 
    if FilePath.is_relative root then
      FilePath.concat (FileUtil.pwd ()) root
    else
      root

  val mutex_watcher = Lwt_mutex.create ()
  val mutable watcher = []

  method root = root

  (** Make the file absolute to root *)
  method rebase fn =
(*       TODO: activate this test  
        if FilePath.is_subdir ctxt.odb.dist_dir fn then *)
    (* TODO: Check fn is a subdir of root *)
    FilePath.make_absolute root fn

  (** Make the file relative to root *)
  method unbase fn =
    FilePath.make_relative root fn

  (** Watchers *)

  (** Add a new watcher *)
  method watch_add ?name f =
    Lwt_mutex.with_lock mutex_watcher
      (fun () ->
         watcher <- (name, f) :: watcher;
         return ())

  (** Remove a watcher, using its names *)
  method watch_remove (name: string) =
    Lwt_mutex.with_lock mutex_watcher
      (fun () ->
         watcher <- 
         List.filter 
           (function 
              | (Some name', _) -> name' <> name
              | (None, _) -> false)
           watcher;
         return ())

  (** Notify watchers of an event *)
  method watch_notify fn ev =
    Lwt_mutex.with_lock mutex_watcher
      (fun () -> 
         return watcher)
    >>= 
    Lwt_list.iter_s (fun (_, f) -> f fn ev)


  (** File management *)

  (** Acquire read resources *)
  method do_read: 'a. (unit -> 'a Lwt.t) -> 'a Lwt.t  =
    fun f -> f ()

  (** Acquire write resources and notify watchers of changes *)
  method do_write: 'a. (unit -> ('a * (filename * event) list) Lwt.t) -> 'a Lwt.t =
    fun f ->
      f () 
      >>= fun (res, ev_lst) ->
      Lwt_list.iter_s 
        (fun (fn, ev) ->
           self#watch_notify fn ev)
        ev_lst
      >>= fun () ->
      return res

  (** Test file existence *)
  method file_exists fn =
    self#do_read
      (fun () ->
         return (Sys.file_exists (self#rebase fn)))

  (** Test if it is a directory *)
  method is_directory fn =
    self#do_read
      (fun () ->
         return (Sys.is_directory (self#rebase fn)))

  (** Traverse a whole filesystem tree *)
  method fold:
      'a. (FileUtilExt.iter_t -> 'a -> 'a Lwt.t) ->
      filename -> 'a -> 'a Lwt.t =
    fun f fn a ->
      self#do_read
        (fun () ->
           FileUtilExt.fold f (self#rebase fn) a)

  (** Traverse a directory *)
  method fold_dir:
      'a. (filename -> filename ->'a -> 'a Lwt.t) ->
      filename -> 'a -> 'a Lwt.t =
    fun f fn a ->
      self#do_read
        (fun () ->
           FileUtilExt.fold_dir f (self#rebase fn) a)

  (** Read from a file *)
  method with_file_in: 'a. filename -> 
      (Lwt_io.input Lwt_io.channel -> 'a Lwt.t) -> 'a Lwt.t = 
    fun fn f ->
      self#do_read 
        (fun () ->
           Lwt_io.with_file ~mode:Lwt_io.input (self#rebase fn) f)

  (** Write to a file *)
  method with_file_out: 'a. filename ->
      (Lwt_io.output Lwt_io.channel -> 'a Lwt.t) -> 'a Lwt.t = 
    fun fn f ->
      self#file_exists fn 
      >>= fun exists_before ->
      self#do_write 
        (fun () -> 
           Lwt_io.with_file ~mode:Lwt_io.output (self#rebase fn) f
           >|= fun res ->
           res, [fn, 
                 if exists_before then 
                   FSChanged 
                 else 
                   FSCreated])

  (** Create a directory with given permissions *)
  method mkdir ?(ignore_exist=false) dn perm =
    self#do_write
      (fun () ->
         catch 
           (fun () ->
              FileUtilExt.mkdir (self#rebase dn) perm
              >|= fun () ->
              (), [dn, FSCreated])
           (function
              | Unix.Unix_error (e, _, _) when e = Unix.EEXIST && ignore_exist ->
                  return ((), [])
              | e ->
                  fail e))

  (** Remove a file or a directory *)
  method rm ?recurse lst =
     self#do_write
      (fun () ->
         FileUtilExt.rm ?recurse (List.map self#rebase lst)
         >|= fun fn_lst ->
         (), List.map (fun fn -> self#unbase fn, FSDeleted) fn_lst)

  (** Move files to the filesystem *)
  method mv other src tgt =
    let mv_op () =
      FileUtilExt.mv 
        (other#rebase src)
        (self#rebase tgt)
      >|= fun (rm_src_lst, rm_tgt_lst, mv_lst) ->
      (List.map
         (fun fn ->
            other#unbase fn, FSDeleted)
         rm_src_lst),

      (List.map
         (fun fn ->
            self#unbase fn, FSDeleted)
         rm_tgt_lst),
      
      mv_lst
    in
      self#do_write
        (fun () ->
           if other == self then
             begin
               mv_op ()
               >|= fun (rm_src_lst, rm_tgt_lst, mv_lst) ->
               (), 
               rm_src_lst
               @
               rm_tgt_lst
               @
               (List.map 
                  (fun (src, tgt) ->
                     self#unbase src, FSMovedTo (self#unbase tgt))
                  mv_lst)
             end
           else
             begin
               other#do_write
                 (fun () ->
                    mv_op ()
                    >|= fun (rm_src_lst, rm_tgt_lst, mv_lst) ->
                    let create_tgt_lst, rm_src_lst' =
                      List.split
                        (List.map
                           (fun (src, tgt) ->
                              (self#unbase tgt, FSCreated),
                              (other#unbase src, FSDeleted))
                           mv_lst)
                    in
                      (rm_tgt_lst @ create_tgt_lst),
                      (rm_src_lst @ rm_src_lst'))
                 >|= fun res ->
                 (), res
             end)

  (** Copy files to the filesystem *)
  method cp other lst tgt =
    let cp_op () =
      FileUtilExt.cp 
        (List.map other#rebase lst)
        (self#rebase tgt)
    in
      self#do_write
        (fun () ->
           if other == self then
             begin
               cp_op ()
               >|= fun cp_lst ->
               (),
               List.map
                 (fun (src, tgt) ->
                    self#unbase tgt, 
                    FSCopiedFrom (self#unbase src))
                 cp_lst
             end
           else
             begin
               other#do_read cp_op 
               >|= fun cp_lst ->
               (),
               List.map
                 (fun (_, tgt) ->
                    self#unbase tgt, FSCreated)
                 cp_lst
             end)

  (** Copy a file descriptor to filesystem *)
  method copy_fd fd tgt =
    self#do_write 
      (fun () ->
         LwtExt.IO.copy_fd fd (self#rebase tgt)
         >>= fun () ->
         return ((), [tgt, FSCreated]))

  (** Return Unix stat of a file *)
  method stat fn =
    self#do_read
      (fun () ->
         try 
           let res = 
             Unix.stat (self#rebase fn)
           in
             return res
         with e ->
           fail e)

end

class rwlock root =
object
  inherit std root as super

  val rwlock = ODBRWLock.create ()

  method do_read f =
    ODBRWLock.with_read_lock rwlock 
      (fun () ->
         super#do_read f)

  method do_write f =
    ODBRWLock.with_write_lock rwlock
      (fun () ->
         super#do_write f)
end

let spy (fs: std) = 
  fs#watch_add 
    (fun fn ev ->
       Lwt_io.eprintl (string_of_event fn ev))
  >>= fun () ->
  return ()
