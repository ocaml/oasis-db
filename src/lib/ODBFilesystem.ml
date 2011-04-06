
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

exception Not_subdir of (filename * filename)

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

(** Read-only filesystem *)
class std_ro root = 
object (self) 

  val root = 
    if FilePath.is_relative root then
      FilePath.concat (FileUtil.pwd ()) root
    else
      root

  method root = root

  (** Make the file absolute to root *)
  method rebase fn =
    let abs_fn = 
      FilePath.make_absolute root fn
    in
      if FilePath.compare root abs_fn = 0 || FilePath.is_subdir abs_fn root then
        abs_fn
      else
        raise (Not_subdir (root, abs_fn))

  method rebase_lwt fn =
    try 
      return (self#rebase fn)
    with e ->
      fail e

  (** Make the file relative to root *)
  method unbase fn =
    FilePath.make_relative root fn

  (** Acquire read resources *)
  method read_begin = 
    return ()

  (** Release read resources *)
  method read_end =
    return ()

  (** Acquire and release resources *)
  method do_read: 'a. (unit -> 'a Lwt.t) -> 'a Lwt.t  =
    fun f ->
      self#read_begin 
      >>= 
      f 
      >>= fun res -> 
      self#read_end 
      >|= fun () ->
      res

  (** Test file existence *)
  method file_exists fn =
    self#do_read
      (fun () ->
         self#rebase_lwt fn >|= Sys.file_exists)

  (** Test if it is a directory *)
  method is_directory fn =
    self#do_read
      (fun () ->
         self#rebase_lwt fn >|= Sys.is_directory)

  (** Open a file for reading *)
  method open_in fn =
    self#read_begin 
    >>= fun () ->
    self#rebase_lwt fn
    >|= fun fn' ->
    (fn,
     Lwt_io.open_file ~mode:Lwt_io.input fn')

  (** Close an input channel *)
  method close_in ((_, chn): filename * Lwt_io.input Lwt_io.channel) =
    Lwt_io.close chn
    >>= fun () ->
    self#read_end

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

(* Do an operation on an input channel *)
let with_file_in fs fn f = 
  fs#open_in fn
  >>= fun ((_, chn) as chn') ->
  f chn
  >>= fun res ->
  fs#close_in chn'
  >|= fun () ->
  res

(** Traverse a whole filesystem tree *)
let fold f fs fn a =
  fs#read_begin
  >>= fun () ->
  (* TODO: maybe the fn can change at each level,
   * maybe call again self#rebase 
   * (e.g. unionfs)
   *)
  fs#rebase_lwt fn
  >>= fun fn' ->
  FileUtilExt.fold f fn' a
  >>= fun res ->
  fs#read_end 
  >|= fun () ->
  res 

(** Traverse a directory *)
let fold_dir f fs fn a =
  fs#read_begin
  >>= fun () ->
  (* TODO: see the TODO of fold *)
  fs#rebase_lwt fn
  >>= fun fn' ->
  FileUtilExt.fold_dir f fn' a
  >>= fun res ->
  fs#read_end
  >|= fun () ->
  res

(** Read-write filesystem *)
class std_rw root = 
object (self) 

  inherit std_ro root

  val mutex_watcher = Lwt_mutex.create ()
  val mutable watcher = []

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

  (** Acquire write resources *)
  method write_begin =
    return ()

  (** Release write resources *)
  method write_end ev_lst =
    Lwt_list.iter_s 
      (fun (fn, ev) ->
         self#watch_notify fn ev)
      ev_lst

  (** Acquire write resources and notify watchers of changes *)
  method do_write: 'a. (unit -> ('a * (filename * event) list) Lwt.t) -> 'a Lwt.t =
    fun f ->
      self#write_begin
      >>= 
      f  
      >>= fun (res, ev_lst) ->
      self#write_end ev_lst
      >>= fun () ->
      return res

  (** Open a file for writing *)
  method open_out fn =
    self#file_exists fn 
    >>= fun exists_before ->
    self#rebase_lwt fn
    >>= fun fn' ->
    self#write_begin 
    >|= fun () ->
    ((exists_before, fn),
     Lwt_io.open_file ~mode:Lwt_io.output fn')

  (** Close a output channel *)
  method close_out ((exists_before, fn), (chn: Lwt_io.output Lwt_io.channel)) =
    Lwt_io.close chn
    >>= fun () ->
    self#write_end 
      [fn, 
       if exists_before then 
         FSChanged 
       else 
         FSCreated]

  (** Create a directory with given permissions *)
  method mkdir ?(ignore_exist=false) dn perm =
    self#do_write
      (fun () ->
         catch 
           (fun () ->
              self#rebase_lwt dn 
              >>= fun fn' ->
              FileUtilExt.mkdir fn' perm
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
         Lwt_list.map_s self#rebase_lwt lst
         >>= fun lst' ->
         FileUtilExt.rm ?recurse lst'
         >|= fun fn_lst ->
         (), List.map (fun fn -> self#unbase fn, FSDeleted) fn_lst)

  (** Move files to the filesystem *)
  method mv other src tgt =
    let mv_op () =
      other#rebase_lwt src
      >>= fun src' ->
      other#rebase_lwt tgt
      >>= fun tgt' ->
      FileUtilExt.mv src' tgt'
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
      Lwt_list.map_s other#rebase_lwt lst
      >>= fun lst' ->
      self#rebase_lwt tgt
      >>= fun tgt' ->
      FileUtilExt.cp lst' tgt'
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
         self#rebase_lwt tgt
         >>= fun tgt' ->
         begin
           let exists_before = 
             Sys.file_exists tgt'
           in
             LwtExt.IO.copy_fd fd tgt'
             >>= fun () ->
             return ((), [tgt, if exists_before then FSChanged else FSCreated])
         end)

end

(** Write to a file *)
let with_file_out fs fn f =
  fs#open_out fn
  >>= fun ((_, chn) as chn') ->
  f chn
  >>= fun res ->
  fs#close_out chn'
  >|= fun () ->
  res


(** Filesystem that uses a rwlock *)
class rwlock root =
object
  inherit std_rw root as super

  val rwlock = ODBRWLock.create ()

  method read_begin =
    ODBRWLock.read_lock rwlock 

  method read_end =
    ODBRWLock.read_unlock rwlock 

  method write_begin =
    ODBRWLock.write_lock rwlock

  method write_end ev_lst =
    ODBRWLock.write_unlock rwlock
    >>= fun () ->
    super#write_end ev_lst
end

let spy (fs: std_rw) = 
  fs#watch_add 
    (fun fn ev ->
       Lwt_io.eprintl (string_of_event fn ev))
  >>= fun () ->
  return ()
