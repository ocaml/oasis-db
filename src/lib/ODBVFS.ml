
(** Filesystem object 
    @author Sylvain Le Gall
  *)

open Lwt
open ODBGettext

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

(* Do an operation on an input channel *)
let with_file_in fs fn f = 
  fs#open_in fn
  >>= fun ((_, chn) as chn') ->
  f chn
  >>= fun res ->
  fs#close_in chn'
  >|= fun () ->
  res


(** Traverse a directory *)
let fold_dir f fs fn a =
  fs#readdir fn 
  >>= fun arr ->
  begin
    let lst = Array.to_list arr in
      Lwt_list.fold_left_s
        (fun a bn ->
           let fn = FilePath.concat fn bn in
             f fn bn a)
        a lst
  end

(** Traverse a whole filesystem tree *)
let fold f fs fn a =
  let rec fold_aux fn _ a = 
    fs#is_directory fn
    >>= fun is_dir ->
      if is_dir then
        begin
          f (`PreDir fn) a
          >>= fun a ->
          fold_dir fold_aux fs fn a
          >>= fun a ->
          f (`PostDir fn) a
        end
      else
        begin
          f (`File fn) a 
        end
  in
    fold_aux fn "" a

(** Write to a file *)
let with_file_out ?flags fs fn f =
  fs#open_out ?flags fn
  >>= fun ((_, chn) as chn') ->
  f chn
  >>= fun res ->
  fs#close_out chn'
  >|= fun () ->
  res

let cp_ext src fs tgt =
  try
    let fd =
      Unix.openfile src [Unix.O_RDONLY] 0o640
    in
      finalize
        (fun () -> 
           fs#copy_fd fd tgt)
        (fun () ->
           return (Unix.close fd))
  with e ->
    fail e

(** Read-only filesystem *)
class virtual read_only = 
object (self) 

  (** Identify the filesystem *)
  method virtual id: string

  (** Give a file prefix that identifiies this FS *)
  method vroot fn =
    Filename.concat self#id fn

  (** Test file existence *)
  method virtual file_exists: filename -> bool Lwt.t

  (** Test if it is a directory *)
  method virtual is_directory: filename -> bool Lwt.t

  (** Open a file for reading, low level *)
  method virtual open_in_low: filename -> ((unit -> unit Lwt.t) * Lwt_io.input_channel) Lwt.t

  (** Open a file for reading *)
  method open_in fn =
    self#open_in_low fn 

  (** Close an input channel *)
  method close_in (close_chn, chn) =
    Lwt_io.close chn
    >>= fun () ->
    close_chn ()

  (** Return Unix stats of a file *)
  method virtual stat: filename -> Unix.LargeFile.stats Lwt.t

  (** Compute digest of a file *)
  method digest fn =
    self#open_in fn 
    >>= fun ((_, chn) as chn') ->
    finalize
      (fun () ->
         LwtExt.IO.digest_chn ~fn chn)
      (fun () ->
         self#close_in chn')

  (** Read a directory content *)
  method virtual readdir: filename -> filename array Lwt.t
end


(** Read-write filesystem *)
class virtual read_write = 
object (self) 

  inherit read_only

  val mutable watcher = []

  (** Watchers *)

  (** Add a new watcher *)
  method watch_add f =
    let id = 
      (List.fold_left 
         (fun id' (id, _) -> max id id')
         0
         watcher) + 1
    in
      watcher <- (id, f) :: watcher;
      id

  (** Remove a watcher, using its names *)
  method watch_remove id =
    watcher <- 
    List.filter 
      (fun (id', _) -> id' <> id)
      watcher

  (** Notify watchers of an event *)
  method watch_notify fn ev =
    Lwt_list.iter_s (fun (_, f) -> f fn ev) watcher

  (** Notify watchers of events *)
  method watch_notify_lst ev_lst =
    Lwt_list.iter_s 
      (fun (fn, ev) ->
         self#watch_notify fn ev)
      ev_lst

  (** Open a file for writing, low level *)
  method virtual open_out_low: ?flags:Unix.open_flag list -> filename ->
      ((unit -> unit Lwt.t) * Lwt_io.output_channel) Lwt.t

  (** Open a file for writing *)
  method open_out ?flags fn  =
    self#file_exists fn
    >>= fun exists_before ->
    self#open_out_low ?flags fn 
    >|= fun (close_fun, chn) ->
    ((exists_before, fn, close_fun), chn)

  (** Close an output channel *)
  method close_out ((exists_before, fn, close_chn), chn) =
    Lwt_io.close chn
    >>= fun () ->
    close_chn () 
    >>= fun () ->
    self#watch_notify
      fn 
      (if exists_before then 
         FSChanged 
       else 
         FSCreated)

  (** Create a directory with given permissions *)
  method virtual mkdir: ?ignore_exist:bool -> filename -> int -> unit Lwt.t

  (** Remove a file *)
  method virtual unlink: filename -> unit Lwt.t

  (** Remove a directory *)
  method virtual rmdir: filename -> unit Lwt.t

  (** Remove a file or a directory *)
  method rm ?(recurse=false) lst =
    Lwt_list.fold_left_s
      (fun acc fn ->
         self#file_exists fn
         >>= fun file_exists ->
           if file_exists then
             fold
               (fun fnt () ->
                  match fnt with
                    | `PreDir fn ->
                        begin
                          return () 
                        end

                    | `PostDir fn ->
                        begin
                          self#rmdir fn
                          >>= fun () ->
                          self#watch_notify fn FSDeleted
                        end

                    | `File fn ->
                        begin
                          self#unlink fn
                          >>= fun () ->
                          self#watch_notify fn FSDeleted
                        end)
               self fn acc
           else
             return acc)
      () lst

  (** Copy files to the filesystem *)
  method cp (other_fs : read_only) lst tgt =

    let notify src tgt =
      let ev =
        if other_fs == (self :> read_only) then
          FSCopiedFrom src
        else 
          FSCreated
      in
        self#watch_notify tgt ev
    in

    let cp_one src tgt =
      (* TODO: take care of directory *)
      (* TODO: fix perms/date *)
      with_file_in other_fs src 
        (fun src_chn ->
          with_file_out self tgt
            (fun tgt_chn ->
               Lwt_io.write_chars tgt_chn
                 (Lwt_io.read_chars src_chn)))
      >>= fun () ->
      notify src tgt
    in

    let rec cp_aux lst tgt =
      self#file_exists tgt 
      >>= fun file_exists ->
      self#is_directory tgt 
      >>= fun is_directory ->
      if file_exists && is_directory then
        Lwt_list.iter_s
          (fun src ->
             let tgt =
               Filename.concat tgt (Filename.basename src)
             in
               cp_aux [src] tgt)
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
    in
      cp_aux lst tgt

  (** Copy a file descriptor to filesystem *)
  method copy_fd fd tgt =
    with_file_out self tgt
      (fun chn ->
         LwtExt.IO.copy_fd_chn fd chn)
end

let spy ?(log=Lwt_io.eprintl) fs = 
  let _i : int =
    fs#watch_add 
      (fun fn ev ->
         log (string_of_event (fs#vroot fn) ev))
  in
    ()
