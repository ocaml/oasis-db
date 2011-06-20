
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
open FileUtilExt
open ODBVFS
open Sexplib.Conv

TYPE_CONV_PATH "ODBSync"

module FS = ODBVFS

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
          failwithf
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

type 'a t =
    {
      sync_rev:          int;
      sync_fs:           (#ODBVFS.read_write as 'a);
      sync_entries:      entry_t list;
      sync_entries_old:  entry_t list;
      sync_map:          (Digest.t * file_size) MapString.t;
      sync_ctxt:         ODBContext.t;
    }

let norm_fn fn =
  FilePath.reduce ~no_symlink:true fn

let id_fn t ?digest fn = 
  t.sync_fs#stat fn
  >>= fun st ->
  return st.Unix.LargeFile.st_size
  >>= fun sz ->
  begin
    match digest with 
      | Some d -> 
          return d
      | None ->
          t.sync_fs#digest fn
  end
  >|= fun digest ->
  digest, sz

let id_fn_ext fn = 
  LwtExt.IO.digest fn
  >>= fun digest ->
  try 
    return (digest, (Unix.LargeFile.stat fn).Unix.LargeFile.st_size)
  with e ->
    fail e

(** Add a file
  *)
let add fn t =
  let fn = norm_fn fn in

  let t' (fn, digest, sz) = 
    {t with
         sync_entries = 
           (Add (fn, digest, sz)) :: t.sync_entries;
         sync_map = 
           MapString.add fn (digest, sz) t.sync_map}
  in
    id_fn t fn 
    >|= fun (digest, sz) ->
    let id = (fn, digest, sz)
    in
      try 
        let reg_digest, reg_sz = 
          MapString.find fn t.sync_map 
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
  let fn = norm_fn fn in

    if MapString.mem fn t.sync_map then 
      {t with 
           sync_entries = 
             (Rm fn) :: t.sync_entries;
           sync_map =
             MapString.remove fn t.sync_map}
    else
      t

(** Filenames of log and log-version
  *)
let fn_meta = "sync-meta.sexp"
let fn_sync = "sync.sexp"

(** Dump the datastructure to disc 
  *)
let dump t =
  let dump_entries chn cnt e = 
    let str = 
      Sexp.to_string_mach
        (sexp_of_v_entry_t (`V1 e))
    in
      Lwt_io.write_line chn str
      >|= fun () -> 
      cnt + 1
  in
  let ctxt = t.sync_ctxt in

  let old_e = t.sync_entries_old in

  let entries = 
    let rec find_unwritten_entries acc new_e =
      if new_e = old_e then
        acc
      else
        begin
          match new_e with 
            | hd :: tl ->
                find_unwritten_entries (hd :: acc) tl 
            | [] ->
                raise Not_found
        end
    in
      try 
        return (find_unwritten_entries [] t.sync_entries)
      with Not_found ->
        fail 
          (Failure 
             (s_ "Unable to find new entries for sync.sexp"))
  in

    entries
    >>= fun entries ->
    with_file_out t.sync_fs
      ~flags:[Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; 
              Unix.O_NONBLOCK]
      fn_sync
      (fun chn ->
         Lwt_list.fold_left_s (dump_entries chn) 0 entries)
    >>= fun cnt ->
    debug ~ctxt (f_ "Added %d entries to file '%s'") cnt fn_sync
    >>= fun () ->
    id_fn t fn_sync
    >>= fun (digest, sz) ->
    with_file_out t.sync_fs fn_meta
      (LwtExt.IO.sexp_dump_chn ~ctxt 
         sexp_of_v_meta_t
         (fun m -> `V1 m)
         ~fn:fn_meta
          {
            sync_meta_rev    = t.sync_rev;
            sync_meta_size   = sz;
            sync_meta_digest = digest;
          })
    >|= fun () ->
    {t with 
         sync_rev         = t.sync_rev;
         sync_entries_old = t.sync_entries}


(** Load datastructure from disc
  *)
let load t = 
  let ctxt = t.sync_ctxt in
    t.sync_fs#file_exists fn_meta 
    >>= fun fn_meta_exists ->
    t.sync_fs#file_exists fn_sync
    >>= fun fn_exists ->
    if fn_meta_exists && fn_exists then
      begin
        with_file_in t.sync_fs fn_meta
          (LwtExt.IO.sexp_load_chn ~ctxt ~fn:fn_meta
             v_meta_t_of_sexp meta_upgrade)
        >>= fun meta ->

        id_fn t fn_sync
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
                   fn_sync fn_sz meta.sync_meta_size)
            else if fn_digest <> meta.sync_meta_digest then
              fail 
                (failwith 
                   (f_ "Checksum mismatch for %s (%s <> %s)") 
                   fn_sync
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
          let init = 
            return t 
          in
            with_file_in t.sync_fs fn_sync
              LwtExt.IO.with_file_content_chn
            >>= fun str ->
            Sexp.scan_fold_sexps
              ~f:rebuild
              ~init
              (Lexing.from_string str)
            >|= fun t ->
            {t with 
                 sync_rev         = meta.sync_meta_rev;
                 sync_entries_old = t.sync_entries}
        end
      end
    else
      begin
        return t
      end

(** Create the datastructure and load it from disc. If there is no datastructure
    on disc, create an empty one.
  *)
let create ~ctxt fs = 
  let res = 
    {
      sync_rev         = 0;
      sync_fs          = fs;
      sync_entries     = [];
      sync_entries_old = [];
      sync_map         = MapString.empty;
      sync_ctxt        = ctxt;
    }
  in
    load res 
    >>= fun res ->
    dump res
    >|= fun res ->
    res

(** Set the appropriate watch on filesystem
  *)
let autoupdate rsync = 
  let watcher fn fse =
    if fn <> fn_sync && fn <> fn_meta then
      begin
        let sync = !rsync in
          sync.sync_fs#is_directory fn
          >>= fun is_dir ->
          if not is_dir then 
            begin
              begin
                match fse with 
                  | FSCreated ->
                      add fn sync
                  | FSDeleted ->
                      return (remove fn sync)
                  | FSChanged ->
                      return sync
                  | FSMovedTo fn' ->
                      add fn' sync >|= remove fn 
                  | FSCopiedFrom fn' ->
                      add fn sync
              end
              >>= 
              dump 
              >>= fun sync ->
              return (rsync := sync)
            end
          else
            return ()
      end
    else
      return ()
  in
  let (_i: int) =
    !rsync.sync_fs#watch_add watcher
  in
    ()

(** Update the sync datastructure with existing data on disc
  *)
let scan rsync =
  FS.fold
    (fun fne acc ->
       match fne with 
         | `File fn when fn <> fn_sync && fn <> fn_meta ->
             return (SetString.add fn acc)
         | _ ->
             return acc)
    !rsync.sync_fs "" SetString.empty
  >>= fun existing_fn ->
  begin
    let sync = !rsync in

    let sync_fn =
      MapString.fold
        (fun fn _ acc ->
           SetString.add fn acc)
        sync.sync_map
        SetString.empty
    in
    let deletes = 
      SetString.diff sync_fn existing_fn
    in
    let adds =
      SetString.diff existing_fn sync_fn
    in
    let sync = 
      SetString.fold remove deletes sync
    in
      SetString.fold 
        (fun fn sync_lwt -> 
           sync_lwt >>= add fn)
        adds (return sync)
      >>= 
      dump 
      >>= fun sync ->
      return (rsync := sync)
  end

class remote sync uri =
object (self)

  inherit FS.read_only as super

  val uri   = uri
  val mutable sync: ODBFSDisk.read_write t  = sync
  val mutable online = SetString.empty

  method ctxt = sync.sync_ctxt
  method cache = sync.sync_fs

  method id = 
    Printf.sprintf "sync_remote(%s, %s)"
      uri self#cache#id

  (** Download an URI to a file using its channel. Use the position
    * of the channel to resume download.
    *)
  method private download_chn url fn chn = 
    let ctxt = self#ctxt in
      debug ~ctxt "Downloading '%s' to '%s'" url fn
      >>= fun () ->
      ODBCurl.Lwt.download_chn url fn chn
      >>= fun () ->
      debug ~ctxt "Download of '%s' to '%s' completed" url fn


  (** Same as [download_chn] but open the file *)
  method private download_fn url fn =
    with_file_out self#cache fn
      (self#download_chn url fn)

  (** Check if the digest of give file is ok
    *)
  method private digest_ok ?(trust_digest=false) fn = 
    self#cache#file_exists fn
    >>= fun exists ->
    if exists then
      begin
        try 
          let (exp_digest, exp_sz) = 
            MapString.find fn sync.sync_map 
          in
          let digest = 
            if trust_digest then 
              Some exp_digest 
            else
              None
          in
            id_fn ?digest sync fn
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
  method private get ?(trust_digest=false) fn = 
      self#digest_ok ~trust_digest fn 
      >>= fun ok ->
      begin
        if not ok then
          begin
            let url = 
              ODBCurl.uri_concat 
                uri 
                (String.concat "/" (explode_filename fn))
            in
              ODBVFS.mkdir 
                self#cache
                ~ignore_exist:true 
                (FilePath.dirname fn)
                0o755
              >>= fun () ->
              info ~ctxt:self#ctxt 
                (f_ "Downloading '%s'") fn
              >>= fun () ->
              self#download_fn url fn
              >>= fun () ->
              self#digest_ok fn
              >>= fun ok ->
              if ok then 
                info ~ctxt:self#ctxt
                  (f_ "Download of '%s' complete")
                  fn
              else
                fail
                  (Failure
                     (Printf.sprintf
                        (f_ "Downloading file '%s' from '%s' doesn't give \
                             the right checksum, update and try again.")
                        fn url))
          end
        else
          return ()
      end

  (* Remove empty directory *)
  method private clean_empty_dir =
    let rec one_pass () = 
      fold 
        (fun e acc ->
           match e with
             | `PostDir dn ->
                 self#cache#readdir dn 
                 >>= 
                 begin
                   function 
                     | [||] -> 
                         info ~ctxt:self#ctxt
                           (f_ "Directory %s is empty")
                           dn
                         >>= fun () ->
                         return (dn :: acc)
                     | _   -> 
                         return acc
                 end
             | _ ->
                 return acc)
        self#cache ""
        []
      >>= 
        function 
          | [] ->
              return ()
          | lst ->
              self#cache#rm ~recurse:true lst
              >>=
              one_pass
    in
      one_pass ()

  method clean_file_filter filter =
    fold
      (fun e lst ->
         match e with 
           | `File fn ->
               if fn <> fn_sync && fn <> fn_meta then 
                 begin
                   filter fn
                   >>= fun ok ->
                   if not ok then 
                     info ~ctxt:self#ctxt 
                       (f_ "File %s doesn't meet clean filter criteria")
                       fn
                     >>= fun () ->
                     return (fn :: lst)
                   else
                     return lst
                 end
               else
                 begin
                   return lst
                 end
           | _ ->
               return lst)
      self#cache ""
      []
    >>=
    self#cache#rm 

  (* Remove files that don't match their digest *)
  method private clean_file_digest = 
    self#clean_file_filter 
      (self#digest_ok ~trust_digest:false)

  (* Remove files that are not in the synchronization data set *)
  method private clean_extra_file =
    self#clean_file_filter
      (fun fn ->
         return (MapString.mem fn sync.sync_map))

  (* Remove files that are set online *)
  method private clean_online_file =
    self#clean_file_filter 
      (fun fn ->
         return (not (SetString.mem fn online)))

  (* Clean the cache file system of all un-needed files *)
  method repair =
    self#clean_online_file
    >>= fun () ->
    self#clean_extra_file
    >>= fun () ->
    self#clean_empty_dir

  (* Set a file to be used online only, i.e. not in the cache *)
  method online_set fn =
    online <- SetString.add fn online

  (* Update the synchronization data, downloading remote data. *)
  method update = 
    let ctxt = self#ctxt in
    let to_url fn = ODBCurl.uri_concat uri (FilePath.basename fn) in
    let uri_meta = to_url fn_meta in
    let uri_sync = to_url fn_sync in


    let check_sync_tmp meta digest sz = 
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

    let download_sync meta_tmp = 
       info ~ctxt
         (f_ "Download synchronization data '%s'")
         uri_sync
       >>= fun () -> 
       LwtExt.IO.MemoryOut.with_file_out
         (fun chn ->
            self#download_chn uri_sync 
              (self#cache#vroot "sync.sexp")
              chn)
       >>= fun str_sync ->
       begin
         let digest = Digest.string str_sync in
         let sz = Int64.of_int (String.length str_sync) in

         let sync_ok, reason = 
           check_sync_tmp meta_tmp digest sz
         in
           if sync_ok then
             info ~ctxt 
               (f_ "Download of '%s' successful.") uri_sync
             >>= fun () ->
             return str_sync
           else
             fail
               (Failure 
                  (Printf.sprintf
                     (f_ "Download of '%s' failed (%s).")
                     uri_sync reason))
       end
    in

      info ~ctxt
        (f_ "Download meta synchronization data '%s'")
        uri_meta;
      >>= fun () ->
      LwtExt.IO.MemoryOut.with_file_out 
        (fun chn ->
           self#download_chn uri_meta 
             (self#cache#vroot "sync-meta.sexp")
             chn)
      >>= fun str_meta ->

      id_fn sync fn_sync
      >>= fun (digest, sz) ->

      LwtExt.IO.sexp_load_str ~ctxt ~fn:(self#cache#vroot "sync-meta.sexp")
        v_meta_t_of_sexp meta_upgrade
        str_meta
      >>= fun meta_tmp ->

      begin
        (* Check that the current file sync.sexp match 
         * the just downloaded sync-meta.sexp.
         *)
        let sync_ok, _ =
          check_sync_tmp meta_tmp digest sz
        in
        let rev_ok =
          sync.sync_rev = meta_tmp.sync_meta_rev
        in
          (* Do we need to download sync.sexp ? *)
          if sync_ok && rev_ok then
            begin
              info ~ctxt
                (f_ "Synchronization data '%s' up to date")
                fn_sync
            end
          else
            begin
              download_sync meta_tmp 
              >>= fun str_sync ->
              (* Install downloaded files to their final destination *)
              ODBVFS.with_file_out self#cache fn_meta 
                (fun chn ->
                   Lwt_io.write chn str_meta)
              >>= fun () ->
              ODBVFS.with_file_out self#cache fn_sync
                (fun chn ->
                   Lwt_io.write chn str_sync)
              >>= fun () -> 
              (* Reload synchronization data *)
              load sync
              >>= fun sync' ->
              begin
                sync <- sync';
                (* Fix obvious problem in the filesystem tree *)
                self#repair
              end
            end
      end

  (** Override of std_ro methods *)

  method file_exists fn =
    let fn = norm_fn fn in
      if FilePath.is_current fn then
        return true
      else if MapString.mem fn sync.sync_map then
        return true
      else
        (* Maybe this is directory *)
        self#is_directory fn

  method is_directory fn =
    let fn = norm_fn fn in
      if FilePath.is_current fn then
        begin
          return true
        end
      else
        begin
          let res = 
            MapString.fold
              (fun fn' _ acc ->
                 let dn = FilePath.dirname fn' in
                   acc || fn = dn)
              sync.sync_map 
              false
          in
            return res
        end

  method open_in_low fn =
    self#get fn
    >>= fun () ->
    self#cache#open_in_low fn

  method stat fn =
    self#get fn
    >>= fun () ->
    self#cache#stat fn

  method readdir dn =
    let dn = norm_fn dn in
    let all = FilePath.is_current dn in

    let head_path fn =
      try
        fst (ExtLib.String.split fn "/")
      with ExtLib.Invalid_string ->
        fn
    in

    let len = String.length dn in

    let sub_prefix fn = 
      String.sub fn (len + 1) ((String.length fn) - len - 1)
    in

    let res =
      MapString.fold
        (fun fn _ acc ->
           if all then
             SetString.add (head_path fn) acc
           else if FilePath.is_subdir fn dn then
             SetString.add (head_path (sub_prefix fn)) acc
           else
             acc)
        sync.sync_map
        SetString.empty
    in
    let lst = SetString.elements res in
      return (Array.of_list lst)

  method real_filename fn =
    self#cache#real_filename fn
end
