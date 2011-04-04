
(** Synchronization of a filesystem tree.
  
    This synchronization use two root files that helps to synchronize the whole
    hierarchy. These files are built in order to minimize the number of HTTP request
    to get them and the amount of data to download.

    The file sync.sexp describes using a list of S-expression what file have
    been added or removed. Each file added comes with its digest/size. This file can
    only grow or the revision number of sync-meta.sexp should be increased.

    @author Sylvain Le Gall
  *)

open Lwt
open Sexplib
open OASISUtils
open ODBGettext
open ODBMessage
open ExtLib

TYPE_CONV_PATH "ODBSync"

type digest = Digest.t

let sexp_of_digest d = 
  Conv.sexp_of_string (Digest.to_hex d)

let digest_of_sexp s = 
  let str = 
    Conv.string_of_sexp s
  in
  let str' =
    assert(String.length str mod 2 = 0);
    String.make ((String.length str) / 2) '\000'
  in
  let code = Char.code in

  let hex_val c =
    match c with 
      | '0'..'9' -> (code c) - (code '0')
      | 'A'..'F' -> (code c) - (code 'A') + 10
      | 'a'..'f' -> (code c) - (code 'a') + 10
      | c ->
          failwithf2 
            (f_ "Unknown hexadecimal digit '%c' in '%s'")
            c str
  in

    for i = 0 to (String.length str') - 1 do 
      str'.[i] <- (Char.chr (hex_val str.[2*i] * 16 + hex_val str.[2*i+1]))
    done;
    str'


type host_filename = string 

(* We dump/load host filename using an OS-independent 
 * format (UNIX) that is converted on the fly when loading
 *)

let host_filename_of_sexp s = 
  let str = Conv.string_of_sexp s in
  let lst = String.nsplit str "/" in
    FilePath.make_filename lst

let rec explode_filename fn =
  let rec explode fn = 
    if FilePath.is_current fn then
      []
    else
      FilePath.basename fn :: explode (FilePath.dirname fn)
  in
    List.rev (explode fn)

let sexp_of_host_filename fn = 
  let str =
    String.concat "/" (explode_filename fn)
  in
    Conv.sexp_of_string str

type file_size = int64 with sexp

type meta_t = 
    {
      sync_meta_rev:    int;
      sync_meta_size:   int64;
      sync_meta_digest: digest;
    }
      with sexp

(**/**)
type v_meta_t = 
    [ `V1 of meta_t ]
      with sexp

let meta_upgrade ~ctxt =
  function
    | `V1 m -> return m
(**/**)

type entry_t = 
  | Add of host_filename * digest * file_size
  | Rm of host_filename
      with sexp

(**/**)
type v_entry_t =
   [ `V1 of entry_t ]
      with sexp

let entry_upgrade ~ctxt = 
  function
    | `V1 e -> return e
(**/**)

module MapString = Map.Make(String)

type t =
    {
      sync_rev:        int;
      sync_base_path:  host_filename;
      sync_entries:    entry_t list;
      sync_map:        (Digest.t * file_size) MapString.t;
    }

let create base_path = 
  let base_path = 
    if FilePath.is_relative base_path then
      FilePath.make_absolute (FileUtil.pwd ()) base_path
    else
      base_path
  in
    {
      sync_rev       = 0;
      sync_base_path = base_path;
      sync_entries   = [];
      sync_map       = MapString.empty;
    }

let relative_fn base_path fn = 
  let make_abs fn = 
    if FilePath.is_relative fn then
      FilePath.make_absolute (FileUtil.pwd ()) fn
    else
      fn
  in
  let base_path = make_abs base_path 
  in
  let fn = make_abs fn 
  in
    FilePath.reduce 
      ~no_symlink:true
      (FilePath.make_relative base_path fn)

let id_fn ?digest fn = 
  try 
    let sz = 
      (Unix.LargeFile.stat fn).Unix.LargeFile.st_size
    in
      begin
        match digest with 
          | Some d -> 
              return d
          | None ->
              LwtExt.IO.digest fn
      end
      >|= fun digest ->
      digest, sz
  with e ->
    fail e


(** Add a file
  *)
let add fn t =

  let t' (rel_fn, digest, sz) = 
    {t with
         sync_entries = 
           (Add (rel_fn, digest, sz)) :: t.sync_entries;
         sync_map = 
           MapString.add fn (digest, sz) t.sync_map}
  in

    id_fn fn 
    >|= fun (digest, sz) ->
    let rel_fn = relative_fn t.sync_base_path fn
    in
    let id = (rel_fn, digest, sz)
    in
      try 
        let reg_digest, reg_sz = 
          MapString.find rel_fn t.sync_map 
        in
          if reg_digest <> digest || reg_sz <> sz then
            t' id
          else
            t
      with Not_found ->
        t' id

(** Remove a file 
  *)
let remove fn t =
  let rel_fn =
    relative_fn t.sync_base_path fn
  in
    if MapString.mem rel_fn t.sync_map then 
      {t with 
           sync_entries = 
             (Rm rel_fn) :: t.sync_entries;
           sync_map =
             MapString.remove rel_fn t.sync_map}
    else
      t

(** Compute the filenames of log and log-version
  *)
let filenames base_path = 
  let rebase = 
    Filename.concat 
      base_path
  in
    rebase "sync-meta.sexp", 
    rebase "sync.sexp"

(** Load the datastructure from disc. If there is no datastructure
    on disc, create an empty one.
  *)
let load ~ctxt base_path =
  let fn_meta, fn = 
    filenames base_path
  in
    if Sys.file_exists fn_meta && Sys.file_exists fn then 
      begin
        LwtExt.IO.sexp_load ~ctxt
          v_meta_t_of_sexp meta_upgrade
          fn_meta
        >>= fun meta ->

        id_fn fn  
        >>= fun (fn_digest, fn_sz) ->

        begin
          let failwith fmt = 
            Printf.ksprintf
              (fun s -> Failure s)
              fmt
          in
            (* Check consistency of the file sync.sexp *)
            if fn_sz <> meta.sync_meta_size then
              fail 
                (failwith 
                   (f_ "Size mismatch for %s (%Ld <> %Ld)") 
                   fn fn_sz meta.sync_meta_size)
            else if fn_digest <> meta.sync_meta_digest then
              fail 
                (failwith 
                   (f_ "Checksum mismatch for %s (%s <> %s)") 
                   fn
                   (Digest.to_hex fn_digest)
                   (Digest.to_hex meta.sync_meta_digest))
            else
              return ()
        end
        >>= fun () ->

        begin
          let rebuild cont sexp = 
            entry_upgrade ~ctxt (v_entry_t_of_sexp sexp)
            >>= fun entry ->
            let f_sync_map =
              match entry with 
                | Add (fn, digest, sz) ->
                    MapString.add fn (digest, sz)
                | Rm fn ->
                    MapString.remove fn 
            in
              cont >|= fun t ->
              {t with 
                   sync_map = f_sync_map t.sync_map;
                   sync_entries = entry :: t.sync_entries}
          in
          let start = 
            return
              {(create base_path) with 
                   sync_rev = meta.sync_meta_rev}
          in
            LwtExt.IO.with_file_content fn
            >>= fun str ->
            Sexp.scan_fold_sexps
              ~f:rebuild
              ~init:start
              (Lexing.from_string str)
        end
      end
    else
      begin
        warning ~ctxt
          (f_ "File '%s' and '%s' not present. Creating empty ODBSync.t") 
          fn_meta fn
        >>= fun () ->
        return (create base_path)
      end

(** Dump the datastructure to disc 
  *)
let dump ~ctxt t =
  let dump_entries chn cnt e = 
    let str = 
      Sexp.to_string_mach
        (sexp_of_v_entry_t (`V1 e))
    in
      Lwt_io.write_line chn str
      >|= fun () -> 
      cnt + 1
  in
  let fn_meta, fn = 
    filenames t.sync_base_path
  in

  let entries = 
    if Sys.file_exists fn then
      (* Reload the previous file to avoid overwriting existing entries *)
      catch 
        (fun () ->
           load ~ctxt t.sync_base_path 
           >>= fun t' ->
             if t'.sync_rev = t.sync_rev then
               begin
                 (* t'.sync_entries should be at the end of t.sync_entries *)
                 let rec drop_prefix =
                   function
                     | (hd1 :: tl1), (hd2 :: tl2) when hd1 = hd2 ->
                         drop_prefix (tl1, tl2)

                     | [], lst ->
                         return lst

                     | _ ->
                         fail 
                           (Failure 
                              (Printf.sprintf 
                                 (f_ "Trying to reuse synchonisation data on \
                                      disc at '%s' that do not match data in \
                                      memory.")
                                 t.sync_base_path))
                 in
                   drop_prefix 
                     ((List.rev t'.sync_entries),
                      (List.rev t.sync_entries))
               end
             else
               return (List.rev t.sync_entries))
        (fun e ->
           warning ~ctxt 
             (f_ "Cannot reload existing synchronisation data in '%s': %s")
             t.sync_base_path
             (Printexc.to_string e)
           >|= fun () ->
           List.rev t.sync_entries)
    else
      return (List.rev t.sync_entries)
  in

    debug ~ctxt (f_ "Creating file '%s'") fn
    >>= fun () ->
    entries
    >>= fun entries ->
    Lwt_io.with_file
      ~flags:[Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; 
              Unix.O_NONBLOCK]
      ~mode:Lwt_io.output fn
      (fun chn ->
         Lwt_list.fold_left_s
           (dump_entries chn)
           0
           entries)
    >>= fun cnt ->
    debug ~ctxt (f_ "Added %d entries to file '%s'") cnt fn
    >>= fun () ->
    id_fn fn 
    >>= fun (digest, sz) ->
    LwtExt.IO.sexp_dump ~ctxt 
      sexp_of_v_meta_t
      (fun m -> `V1 m)
      fn_meta
      {
        sync_meta_rev    = t.sync_rev;
        sync_meta_size   = sz;
        sync_meta_digest = digest;
      }

module SetSync = 
  Set.Make 
    (struct
       type t = host_filename * digest * file_size
       let compare (fn1, dgst1, sz1) (fn2, dgst2, sz2) = 
         match FilePath.compare fn1 fn2 with 
           | 0 ->
               begin
                 match String.compare dgst1 dgst2 with 
                   | 0 ->
                       Int64.compare sz1 sz2
                   | n ->
                       n
               end
           | n ->
               n
     end)

module Remote =
struct 

  type sync = t

  type t = 
      {
        sync_cache_dir: host_filename;
        sync_uri:       string;
        sync_method:    [`Full | `Cached];
        sync_data:      sync;
      }

  let create ~ctxt cache_dir uri = 
    load ~ctxt cache_dir 
    >>= fun sync ->
    return 
      {
        sync_cache_dir = cache_dir;
        sync_uri       = uri;
        sync_method    = `Full;
        sync_data      = sync;
      }

  (** [url_concat url path] Concatenates a relative [path] onto an absolute
    * [url] 
    *)
  let url_concat url tl = 
    (* TODO: cover more case  of URL concat *)
    if String.ends_with url "/" then
      url^tl
    else
      url^"/"^tl

  (** Take care of creating curl socket and closing it
    *)
  let with_curl f = 
    (* Generic init of curl *)
    let c = 
      Curl.init () 
    in
    let () = 
      Curl.set_failonerror c true
    in
      finalize 
        (fun () -> 
           f c)
        (fun () -> 
           Curl.cleanup c;
           return ())

  (** Download an URI to a file using its channel. Use the position
    * of the channel to resume download.
    *)
  let download_chn ~ctxt ?curl url fn chn = 
    let curl_write fn chn d = 
      output_string chn d;
      String.length d
    in

    let download_curl c = 
      try 
        (* Resume download *)
        Curl.set_url c url;
        Curl.set_writefunction c (curl_write fn chn);
        Curl.set_resumefromlarge c (LargeFile.pos_out chn);
        Curl.perform c;
        return ()
      with e -> 
        fail e 
    in

      debug ~ctxt "Downloading '%s' to '%s'" url fn
      >>= fun () ->
      begin
        match curl with 
          | Some c ->
              download_curl c
          | None ->
              with_curl download_curl
      end
      >>= fun () ->
      debug ~ctxt "Download of '%s' to '%s' completed" url fn


  (** Same as [download_chn] but open the file *)
  let download_fn ~ctxt ?curl url fn =
    let chn =
      open_out fn
    in
      finalize
        (fun () ->
           download_chn ~ctxt ?curl url fn chn)
        (fun () ->
           close_out chn;
           return ())

  (** Check if the digest of give file is ok
    *)
  let digest_ok ~ctxt ?(trust_digest=false) t fn = 
    let fn_disk = 
      FilePath.concat t.sync_cache_dir fn
    in
      if Sys.file_exists fn_disk then
        begin
          try 
            let (exp_digest, exp_sz) = 
              MapString.find fn t.sync_data.sync_map 
            in
            let digest = 
              if trust_digest then 
                Some exp_digest 
              else
                None
            in
              id_fn ?digest fn_disk 
              >|= fun (digest, sz) ->
              digest = exp_digest && sz = exp_sz
          with Not_found ->
            fail 
              (Failure 
                 (Printf.sprintf 
                    (f_ "File '%s' is not part of the synchronization data")
                    fn))
        end
      else
        begin
          return false
        end

  (** Compute the host filename on disk, of a repository filename. It also 
    * makes sure that digest/size match expectation otherwise download it
    * from repository
    *)
  let get ~ctxt ?curl ?(trust_digest=false) t fn = 
    let fn_disk = 
      FilePath.concat t.sync_cache_dir fn
    in
      digest_ok ~ctxt ~trust_digest t fn 
      >>= fun ok ->
      begin
        if not ok then
          begin
            let url = 
              url_concat t.sync_uri (String.concat "/" (explode_filename fn))
            in
              FileUtilExt.mkdir ~ignore_exist:true 
                (FilePath.dirname fn_disk) 0o755
              >>= fun () ->
              download_fn ~ctxt ?curl url fn_disk 
          end
        else
          return ()
      end
      >>= fun () ->

      return fn_disk

  (* Remove empty directory *)
  let clean_empty_dir ~ctxt t =
    FileUtilExt.iter
      (function
         | FileUtilExt.PostDir dn ->
             if Sys.readdir dn = [||] then
               FileUtilExt.rm ~recurse:true [dn]
               >|= ignore
             else
               return ()
         | _ ->
             return ())
      t.sync_cache_dir


  (* Remove files that don't match their digest *)
  let clean_file_digest ~ctxt t = 
    FileUtilExt.fold
      (fun e lst ->
         match e with 
           | FileUtilExt.File fn ->
               digest_ok ~ctxt ~trust_digest:false t 
                 (relative_fn t.sync_cache_dir fn)
               >>= fun ok ->
               if not ok then 
                 return (fn :: lst)
               else
                 return lst
           | _ ->
               return lst)
      t.sync_cache_dir
      []
    >>=
    FileUtilExt.rm 
    >|= ignore

  let filter_sync_method t fn = 
    match t.sync_method with 
      | `Full ->
          true

      | `Cached ->
          (* We only synchronize important data: _oasis and 
           * storage.sexp 
           *)
          begin
            match FilePath.basename fn with 
              | "storage.sexp"
              | "_oasis" -> 
                  true

              | _ ->
                  false
          end

  let repair ~ctxt ?(trust_digest=false) ?(remove_extra=false) t =
    (* If we change sync method, there should be some extra files
     * all around the FS, remove them
     *)
    begin
      if remove_extra then
        FileUtilExt.fold
          (fun e lst ->
             match e with 
               | FileUtilExt.File fn ->
                   begin
                     let rel_fn = 
                       relative_fn t.sync_cache_dir fn 
                     in
                     let lst' =
                       if not (filter_sync_method t rel_fn) then
                         fn :: lst
                       else
                         lst
                     in
                       return lst'
                   end

               | _ ->
                   return lst)
          t.sync_cache_dir 
          []
        >>= 
        FileUtilExt.rm 
        >|= ignore
      else
        return ()
    end
    >>= fun () ->

    (* Check that digest match file system *)
    begin
      if not trust_digest then
        clean_file_digest ~ctxt t
      else
        return ()
    end
    >>= fun () ->

    (* Add missing files using sync_data *)
    begin
      let downloads = 
        MapString.fold
          (fun fn _ lst ->
             if filter_sync_method t fn then
               fn :: lst
             else
               lst)
          t.sync_data.sync_map 
          []
      in
        Lwt_list.iter_s
          (fun fn -> 
             get ~ctxt ~trust_digest:true t fn
             >|= fun _ ->
             ())
          downloads
    end
    >>= fun () ->

    (* Remove empty directories *)
    clean_empty_dir ~ctxt t


  let update ~ctxt t = 
    let fn_meta, fn = 
      filenames t.sync_cache_dir 
    in
    let to_url fn =
      url_concat t.sync_uri (FilePath.basename fn)
    in
    let url_meta = to_url fn_meta in
    let url = to_url fn in

    (* Download sync-meta.sexp *)
    let fn_meta_tmp, chn_meta_tmp = 
      Filename.open_temp_file "oasis-db-sync-meta-" ".sexp"
    in
    let fn_tmp, chn_tmp  = 
      Filename.open_temp_file "oasis-db-sync-" ".sexp"
    in

    let clean () = 
      let safe_close chn = 
        try close_out chn with _ -> ()
      in
        safe_close chn_meta_tmp;
        safe_close chn_tmp;
        FileUtilExt.rm [fn_meta_tmp; fn_tmp]
        >|= ignore
    in

    let check_sync_tmp meta fn_tmp = 
      id_fn fn_tmp 
      >|= fun (digest, sz) ->
      (* Check downloaded data *)
      if meta.sync_meta_size = sz && meta.sync_meta_digest = digest then
        true, ""
      else if meta.sync_meta_size <> sz && meta.sync_meta_digest <> digest then
        false, 
        Printf.sprintf
          (f_ "size: %Ld <> %Ld; digest: %s <> %s")
          meta.sync_meta_size sz
          (Digest.to_hex meta.sync_meta_digest)
          (Digest.to_hex digest)
      else if meta.sync_meta_size <> sz then
        false, 
        Printf.sprintf
          (f_ "size: %Ld <> %Ld")
          meta.sync_meta_size sz
      else 
        false,
        Printf.sprintf
          (f_ "digest: %s <> %s")
          (Digest.to_hex meta.sync_meta_digest)
          (Digest.to_hex digest)
    in

    let proceed curl = 
      load ~ctxt t.sync_cache_dir
      >>= fun sync_old ->

      begin
        if Sys.file_exists fn_meta then
          begin
            info ~ctxt 
              (f_ "Load current meta synchronization data '%s'")
              fn_meta
            >>= fun () ->
            LwtExt.IO.sexp_load ~ctxt
              v_meta_t_of_sexp meta_upgrade
              fn_meta
            >|= fun meta_old ->
            Some meta_old
          end
        else
          return None
      end
      >>= fun meta_old_opt ->
      finalize
        (fun () -> 
           info ~ctxt 
             (f_ "Download meta synchronization data '%s'")
             url_meta;
           >>= fun () ->
           download_chn ~ctxt ~curl url_meta fn_meta_tmp chn_meta_tmp
           >>= fun () ->
           return (close_out chn_meta_tmp)
           >>= fun () ->
           LwtExt.IO.sexp_load ~ctxt
             v_meta_t_of_sexp meta_upgrade
             fn_meta_tmp
           >>= fun meta ->

           (* Do a copy of current sync.sexp if the update condition stands *)
           begin
             let cond_update = 
               match meta_old_opt with 
                 | Some meta_old ->
                     meta_old.sync_meta_rev = meta.sync_meta_rev 
                 | None ->
                     false
             in
               if cond_update then
                 begin
                   let lwt_chn = 
                     Lwt_io.of_unix_fd
                       ~mode:Lwt_io.output
                       (Unix.dup (Unix.descr_of_out_channel chn_tmp))
                   in
                     Lwt_io.with_file ~mode:Lwt_io.input fn
                       (fun chn_in ->
                          Lwt_io.write_lines lwt_chn
                            (Lwt_io.read_lines chn_in))
                     >>= fun () ->
                     Lwt_io.close lwt_chn
                     >|= fun () ->
                     begin
                       (* Seek to end of file *)
                       LargeFile.seek_out 
                         chn_tmp
                         (LargeFile.out_channel_length 
                            chn_tmp);
                     end
                 end
               else
                 begin
                   return ()
                 end
           end
           >>= fun () ->

           (* Download the missing bytes at the end of the current sync.sexp *)
           begin
             let update_start = 
               LargeFile.pos_out chn_tmp
             in
             let update_length =
               Int64.sub meta.sync_meta_size update_start
             in
               if update_length >= 0L then 
                 begin
                   info ~ctxt 
                     (f_ "Need to fetch %LdB from '%s' (data already donwloaded: %LdB)")
                     update_length url update_start 
                   >>= fun () ->
                   (* Resume download *)
                   begin
                     if update_length > 0L then
                       download_chn ~ctxt ~curl url fn_tmp chn_tmp
                     else
                       return ()
                   end
                   >>= fun () ->
                   begin
                     close_out chn_tmp;
                     return ()
                   end
                 end
               else 
                 begin
                   fail 
                     (Failure
                        (Printf.sprintf 
                           (f_ "Data already downloaded %LdB, data to \
                                download %LdB: inconsistent sizes. \
                                The file at URL '%s' should only grow, \
                                contact administrator.")
                           update_start meta.sync_meta_size url))
                 end
           end
           >>= fun () ->

           (* Check that missing bytes + current content match the digest of
            * sync.sexp
            *)
           check_sync_tmp meta fn_tmp 
           >>= fun (sync_ok, reason) ->
           begin
             (* Check downloaded data *)
             if sync_ok then
               (* We got the good file *)
               info ~ctxt (f_ "Download of '%s' successful.") url
             else 
               begin
                 warning ~ctxt 
                   (f_ "Download of '%s' failed (%s), \
                        falling back to full download")
                   url reason
                 >>= fun () -> 
                 begin
                   let chn_tmp =
                     open_out fn_tmp 
                   in
                     finalize
                       (fun () -> 
                          download_chn ~ctxt ~curl url fn_tmp chn_tmp)
                       (fun () ->
                          close_out chn_tmp;
                          return ())
                     >>= fun () ->
                     check_sync_tmp meta fn_tmp
                     >>= fun (sync_ok, reason) ->
                     begin
                       if sync_ok then
                         info ~ctxt 
                           (f_ "Full download of '%s' successful.") url
                       else
                         fail
                           (Failure 
                              (Printf.sprintf
                                 (f_ "Full download of '%s' failed (%s).")
                                 url reason))
                     end
                 end
               end

           end
           >>= fun () ->

           (* We reach this point, all files should be valid. Copy them to their
            * final destination 
            *)
           Lwt.join
             [
               FileUtilExt.cp [fn_meta_tmp] fn_meta >|= ignore;
               FileUtilExt.cp [fn_tmp] fn >|= ignore;
             ]
        )

        (* Always clean at the end *)
        clean

      >>= fun () -> 
      (* We should have synchronized synchronization data, now start
       * real data synchronization: tarball, storage.sexp et al
       *)
      load ~ctxt t.sync_cache_dir
      >>= fun sync_new ->

      (* Compare the content of sync_old and sync_new and guess what operation
       * need to be done.
       *)
      begin
        let map_to_set sync =
          MapString.fold
            (fun k (digest, sz) ->
               SetSync.add (k, digest, sz))
            sync.sync_map
            SetSync.empty
        in
        let set_to_files st = 
          SetSync.fold 
            (fun (fn, _, _) lst -> fn :: lst)
            st
            []
        in

        let fs_new = map_to_set sync_new in
        let fs_old = map_to_set sync_old in

        (* What should be removed *)
        let deletes = 
          set_to_files (SetSync.diff fs_old fs_new)
        in

        (* What should be added/updated *)
        let adds = 
          set_to_files (SetSync.diff fs_new fs_old)
        in

          (* Remove old files *)
          FileUtilExt.rm
            (List.rev_map (FilePath.concat t.sync_cache_dir) deletes)
          >>= fun _ ->
          Lwt_list.iter_s 
            (fun fn ->
               if filter_sync_method t fn then
                 get ~ctxt t fn
                 >|= fun (_s : string) ->
                 ()
               else
                 return ())
            adds
          >>= fun () ->
          (* Check everything on the FS *)
          repair ~ctxt ~trust_digest:true t
      end

    in
      with_curl proceed
end
