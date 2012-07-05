
(** Maintain a cache of data based on a file.

    This module allow to create a cache of data that has been parsed from a
    file. It do so by keeping the file status.
  *)

open ODBTypes
open Lwt

type 'a t = (filename, (Unix.LargeFile.stats option * 'a)) Hashtbl.t

let create () = 
  Hashtbl.create 13

let get t vfs fn f = 
  catch 
    (fun () ->
       vfs#stat fn 
       >|= fun stat ->
       Some {stat with Unix.LargeFile.st_atime = 0.0})
    (fun _ ->
       return None)
  >>= fun stat_opt_now ->
  let update () =
    begin
      if stat_opt_now <> None then
        ODBVFS.with_file_in vfs fn (fun chn -> f (Some chn))
      else
        f None 
    end
    >|= fun data_now ->
    Hashtbl.replace t fn (stat_opt_now, data_now);
    data_now
  in
    if Hashtbl.mem t fn then 
      begin
        let stat_opt_before, data = Hashtbl.find t fn in
          if stat_opt_now <> stat_opt_before then
            update ()
          else
            return data
      end
    else
      begin
        update ()
      end

let remove t fn =
  Hashtbl.remove t fn
