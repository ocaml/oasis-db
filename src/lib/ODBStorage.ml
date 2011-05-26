
open ODBMessage  
open ODBGettext
open ODBTypes
open ODBInotify
open ODBContext
open ODBUtils
open Lwt

module HLS = ODBHLS
module FS = ODBFilesystem

(** Storage information for a version 
  *)
type pkg_ver_t =
  {
    pkg_ver: ODBPkgVer.t;
    pkg_ver_dir: dirname;

    pkg_ver_oasis_rwlock: ODBRWLock.t;
    mutable pkg_ver_oasis: 
      [ `Set of OASISTypes.package 
      | `Not_set
      | `Not_found
      | `Error]
  }

(** Storage information for a package
  *)
type pkg_t = 
  {
    pkg:        ODBPkg.t;
    pkg_dir:    dirname;

    pkg_latest_rwlock: ODBRWLock.t;
    mutable pkg_latest: ODBPkgVer.t option;

    pkg_vers: pkg_ver_t HLS.t
  }

(** Main datastructure *)
type 'a t = 
    {
      stor_fs:   'a;
      stor_all:  pkg_t HLS.t;
      stor_ctxt: ODBContext.t;
    } constraint 'a = #FS.std_ro

type rw_t = FS.std_rw t

type ver_str = string
type pkg_str = string
type filename = string
type dirname = string

exception FileDoesntExist 

(* Name of the file handling ODBStorage data for a Pkg or a PkgVer *)
let storage_filename dn =
  Filename.concat dn "storage.sexp"

let check_list ~ctxt msg lst = 
  let proceed, lst =
    List.fold_left
      (fun (proceed, lst) (cond, is_error, msg) ->
        if cond then
          if is_error then
            false, (error ~ctxt "%s" msg :: lst)
          else
            proceed, (warning ~ctxt "%s" msg :: lst)
        else
          proceed, lst)
      (true, [])
      lst
  in
    join lst 
    >>= fun () ->
    if proceed then 
      return ()
    else
      fail (Failure msg)

module type OPS =
sig
  type key_up
  type key_cur
  type container_t
  type element_t
  type file_type_t

  val key_up_of_key_cur: key_cur -> key_up
  val key_str: key_cur -> string
  val hls_get: 'a t -> key_up -> container_t HLS.t Lwt.t
  val to_element: container_t -> element_t
  val to_dir: container_t -> dirname
  val to_fn: element_t -> file_type_t -> filename
end

module Common (O: OPS) =
struct
  let hls_get =
    O.hls_get 

  let get t k =
    O.hls_get t (O.key_up_of_key_cur k)

  (* See ODBStorage.mli *)
  let elements t ku = 
    O.hls_get t ku
    >>= 
    HLS.elements
    >|= 
    List.map (fun (_, cont) -> O.to_element cont)

  (* See ODBStorage.mli *)
  let mem t k = 
    catch 
      (fun () ->
         get t k 
         >>= fun hls ->
         HLS.mem hls (O.key_str k))
      (function
         | Not_found ->
             return false
         | e ->
             fail e)

  let find_container t k =
    get t k
    >>= fun hls ->
    HLS.find hls (O.key_str k)

  (* See ODBStorage.mli *)
  let find t k =
    find_container t k
    >|= fun container -> 
    O.to_element container

  (* Return dirname and element *)
  let dirname_and_element t k = 
    find_container t k
    >>= fun cont ->
    return (O.to_element cont, O.to_dir cont)

  (* See ODBStorage.mli *)
  let dirname t k = 
    dirname_and_element t k
    >|= fun (_, dn) ->
    dn

  (* See ODBStorage.mli *)
  let filename t k fn =
    dirname_and_element t k 
    >|= fun (e, dn) ->
    begin
      let bn = 
        O.to_fn e fn
      in
        Filename.concat dn bn
    end

  (* See ODBStorage.mli *)
  let with_file_in ?(catch=(fun e -> fail e)) t k fn read =
    filename t k fn 
    >>= fun fn ->
    t.stor_fs#file_exists fn
    >>= fun exists ->
    if exists then
      Lwt.catch 
        (fun () ->
           FS.with_file_in t.stor_fs fn read)
        catch
    else
      catch FileDoesntExist

  (* See ODBStorage.mli *)
  let with_file_out t k fn f =
    filename t k fn 
    >>= fun fn ->
    FS.with_file_out t.stor_fs fn f 

  (* See ODBStorage.mli *)
  let file_exists t k fn =
    filename t k fn
    >>= fun fn' ->
    t.stor_fs#file_exists fn'
end

module Pkg = 
struct
  open ODBPkg 

  type file_type = 
      [ `Other of string 
      | `PluginData of string]

  type key =
      [ `Str of pkg_str
      | `Pkg of t
      | `PkgVer of ODBPkgVer.t]

  module Common = 
    Common
      (struct
         type key_up = unit
         type key_cur = key
         type container_t = pkg_t
         type element_t = ODBPkg.t
         type file_type_t = file_type

         let key_up_of_key_cur = ignore

         let key_str =
           function
             | `Str s -> s
             | `Pkg t -> t.pkg_name
             | `PkgVer pkg_ver -> pkg_ver.ODBPkgVer.pkg

         let hls_get t _ = return t.stor_all
         let to_element cont = cont.pkg
         let to_dir cont = cont.pkg_dir
         let to_fn _ =
           function 
             | `PluginData plg -> plg^".sexp"
             | `Other fn -> fn
       end)

  include Common

  (* See ODBStorage.mli *)
  let elements t = 
    elements t ()

  (* Add a package that is already in the filesystem *)
  let add ~ctxt t dn =  
    let storage_fn = 
      storage_filename dn 
    in
      FS.with_file_in t.stor_fs storage_fn
        (ODBPkg.from_chn ~ctxt ~fn:storage_fn)
      >>= fun ({pkg_name = pkg_str} as pkg) ->
      HLS.mem t.stor_all pkg_str
      >>= fun pkg_exists ->
      begin
        let bn = 
          Filename.basename dn
        in
          check_list ~ctxt 
            (spf 
               (f_ "Adding package '%s' from directory '%s'")
               pkg_str dn)
            [
              bn <> pkg_str,
              false,
              spf (f_ "Storage file '%s' matches package '%s' but is in \
                       directory '%s'")
                storage_fn pkg_str bn;

              pkg_exists,
              true,
              spf (f_ "Storage file '%s' defines package '%s' previously \
                       defined.") 
                storage_fn pkg_str;
            ]
      end 
      >>= fun () ->
      t.stor_fs#stat storage_fn
      >>= fun stat ->
      begin
        let date =
          CalendarLib.Calendar.from_unixfloat
            stat.Unix.LargeFile.st_mtime
        in
        let pkg_t = 
          {
            pkg      = pkg;
            pkg_dir  = dn;
            pkg_vers = HLS.create ();

            pkg_latest = None;
            pkg_latest_rwlock = ODBRWLock.create ();
          }
        in
          HLS.add t.stor_all pkg_str pkg_t
          >|= fun () ->
          (date, `Pkg (pkg_str, `Created), pkg)
      end

  (* See ODBStorage.mli *)
  let create ~ctxt (t: rw_t) pkg = 
    let pkg_str = 
      pkg.pkg_name
    in
      t.stor_fs#mkdir pkg_str 0o755
      >>= fun () ->
      catch 
        (fun () ->
           begin
             let fn = 
               storage_filename pkg_str
             in
               FS.with_file_out 
                 t.stor_fs fn
                 (ODBPkg.to_chn ~ctxt ~fn pkg)
           end
           >>= fun () ->
           add ~ctxt t pkg.pkg_name)
        (fun e ->
           t.stor_fs#rm ~recurse:true [pkg_str]
           >>= fun () ->
           fail e)
end

module PkgVer = 
struct 

  open ODBPkgVer

  type file_type =
    [ `OASIS
    | `OASISPristine
    | `Other of filename
    | `PluginData of string
    | `Tarball ]

  type key = 
      [ `Str of pkg_str * ver_str
      | `StrVer of pkg_str * OASISVersion.t
      | `PkgVer of ODBPkgVer.t]

  module Common =
    Common
      (struct
         type key_up = Pkg.key
         type key_cur = key
         type container_t = pkg_ver_t
         type element_t = ODBPkgVer.t
         type file_type_t = file_type

         let key_up_of_key_cur =
           function
             | `Str (pkg_str, _) 
             | `StrVer (pkg_str, _)
             | `PkgVer {pkg = pkg_str} -> 
                 `Str pkg_str

         let key_str =
           function
             | `Str (_, ver_str) -> 
                 ver_str
             | `StrVer (_, ver) ->
                 OASISVersion.string_of_version ver
             | `PkgVer pkg_ver -> 
                 OASISVersion.string_of_version pkg_ver.ver

         let hls_get t ku =
           Pkg.find_container t ku
           >>= fun pkg_strg ->
           return pkg_strg.pkg_vers

         let to_element cont = cont.pkg_ver
         let to_dir cont = cont.pkg_ver_dir
         let to_fn e = 
           function 
             | `OASIS -> "_oasis"
             | `OASISPristine -> "_oasis.pristine"
             | `Tarball -> e.tarball
             | `PluginData nm -> nm^".sexp"
             | `Other fn -> fn
       end)

  include Common

  (* See ODBStorage.mli *)
  let elements ?extra t pkg_k = 
    elements t pkg_k
    >|= fun lst ->
    begin
      let lst = 
        match extra with 
          | Some pkg_ver -> 
              if List.exists 
                   (fun pkg_ver' ->
                      pkg_ver'.ODBPkgVer.ver = pkg_ver.ODBPkgVer.ver &&
                      pkg_ver'.ODBPkgVer.pkg = pkg_ver.ODBPkgVer.pkg)
                   lst then
                lst
              else
                pkg_ver :: lst
          | None -> lst
      in
        List.sort ODBPkgVer.compare lst
    end
    
  (* See ODBStorage.mli *)
  let latest ?extra t ku = 
    Pkg.find_container t ku
    >>= fun pkg_cont ->
    ODBRWLock.with_read_lock 
      pkg_cont.pkg_latest_rwlock
      (fun () ->
         match pkg_cont.pkg_latest, extra with
           | Some e, Some e' ->
               begin
                 try 
                   if ODBPkgVer.compare e e' < 0 then
                     begin
                       return e'
                     end
                   else
                     begin
                       return e
                     end
                 with e ->
                   fail e
               end

           | None, Some e 
           | Some e, None ->
               return e
           | None, None ->
               fail Not_found)

  (** Add a package version that is already in the dist_dir 
    *)
  let add ~ctxt t pkg_str dn = 
    let storage_fn = 
      storage_filename dn 
    in
      FS.with_file_in t.stor_fs storage_fn 
        (ODBPkgVer.from_chn ~ctxt ~fn:storage_fn)
      >>= fun pkg_ver ->
      return (OASISVersion.string_of_version pkg_ver.ODBPkgVer.ver)
      >>= fun ver_str ->
      Pkg.find_container t (`Str pkg_str)
      >>= fun pkg_cont ->
      HLS.mem pkg_cont.pkg_vers ver_str
      >>= fun ver_exists ->
      Pkg.dirname t (`Str pkg_str)
      >>= fun pkg_dn ->
      begin
        let bn = 
          Filename.basename dn 
        in
          check_list ~ctxt
            (spf 
               (f_ "Adding version '%s' to package '%s' from directory '%s'")
               ver_str pkg_str dn)
            [
              FilePath.is_subdir pkg_dn dn,
              false,
              spf (f_ "Directory '%s' is not a subdirectory of package dir '%s'")
                dn pkg_dn;

              bn <> ver_str,
              false,
              spf (f_ "Storage file '%s' is version '%s' but is contained \
                       in directory '%s'") storage_fn ver_str bn;

              ver_exists,
              false,
              spf (f_ "Storage file '%s' defines version '%s' previously \
                       defined.") storage_fn ver_str;

              pkg_ver.ODBPkgVer.pkg <> pkg_str,
              true, 
              spf (f_ "Storage file '%s' belongs to package '%s' but \
                       is stored in package '%s'.")
                storage_fn pkg_ver.ODBPkgVer.pkg pkg_str;
            ]
      end
      >>= fun () ->
      begin
        let pkg_ver_t = 
          {
            pkg_ver              = pkg_ver;
            pkg_ver_dir          = dn;
            pkg_ver_oasis        = `Not_set;
            pkg_ver_oasis_rwlock = ODBRWLock.create ();
          }
        in
          latest ~extra:pkg_ver t (`Str pkg_str)
          >>= fun e ->
          begin
            if e = pkg_ver then
              ODBRWLock.with_write_lock 
                pkg_cont.pkg_latest_rwlock
                (fun () ->
                   return (pkg_cont.pkg_latest <- Some pkg_ver))
            else
              return ()
          end
          >>= fun () ->
          HLS.add pkg_cont.pkg_vers ver_str pkg_ver_t
          >|= fun () ->
          (pkg_ver.ODBPkgVer.upload_date,
           `Pkg (pkg_ver.ODBPkgVer.pkg, 
                 `VersionCreated pkg_ver.ODBPkgVer.ver),
           pkg_ver)
      end

  (* See ODBStorage.mli *)
  let create ~ctxt (t: rw_t) pkg_ver tarball_fd = 
    catch 
      (fun () -> 
         Pkg.dirname t (`PkgVer pkg_ver))
      (function
         | Not_found as e -> 
             error ~ctxt 
               (f_ "Package '%s' doesn't exist") 
               pkg_ver.ODBPkgVer.pkg
             >>= fun () ->
             fail e
         | e ->
             fail e)
    >>= fun pkg_dn ->
    begin
      let dn = 
        Filename.concat pkg_dn 
          (OASISVersion.string_of_version 
             pkg_ver.ODBPkgVer.ver)
      in
        t.stor_fs#mkdir dn 0o755 
        >>= fun () ->
        catch 
          (fun () ->
             (* Create storage.sexp *)
             begin
               let fn =  storage_filename dn in
                 FS.with_file_out 
                   t.stor_fs fn
                   (ODBPkgVer.to_chn ~ctxt ~fn pkg_ver)
             end
             >>= fun () ->

             (* Copy the tarball *)
             t.stor_fs#copy_fd
               tarball_fd 
               (Filename.concat dn pkg_ver.ODBPkgVer.tarball)
             >>= fun () ->

             (* Notify installation of a new package version *)
             add ~ctxt t pkg_ver.ODBPkgVer.pkg dn)

          (fun e ->
             t.stor_fs#rm ~recurse:true [dn]
             >>= fun _ ->
             fail e)
    end

  let oasis_load t k =
    filename t k `OASIS
    >>= fun fn ->
    with_file_in t k `OASIS
      ~catch:(function 
                | FileDoesntExist ->
                    return `Not_found
                | e ->
                    return `Error)
      (fun chn ->
         ODBOASIS.from_chn ~ctxt:t.stor_ctxt ~fn:(t.stor_fs#rebase fn) chn
         >|= fun oasis ->
         `Set oasis)
    >>= fun oasis_set ->
    find_container t k
    >>= fun pkg_ver_cont ->
    ODBRWLock.with_write_lock 
      pkg_ver_cont.pkg_ver_oasis_rwlock
      (fun () ->
         return (pkg_ver_cont.pkg_ver_oasis <- oasis_set))

  let with_file_out t k fn write =
    finalize
      (fun () -> 
         with_file_out t k fn write)
      (fun () ->
         find_container t k 
         >>= fun pkg_ver_cont ->
         ODBRWLock.with_write_lock
           pkg_ver_cont.pkg_ver_oasis_rwlock
           (fun () ->
              return (pkg_ver_cont.pkg_ver_oasis <- `Not_set)))

  let rec oasis_get t k =
    find_container t k 
    >>= fun pkg_ver_cont ->
    ODBRWLock.with_read_lock 
      pkg_ver_cont.pkg_ver_oasis_rwlock
      (fun () -> return pkg_ver_cont.pkg_ver_oasis)
    >>= 
      function
        | `Set _ | `Not_found | `Error as e ->
            return e
        | `Not_set ->
            oasis_load t k
            >>= fun () ->
            oasis_get t k

  let rec oasis t k = 
    oasis_get t k
    >|= 
      function
        | `Set oasis -> 
            Some oasis
        | `Not_found | `Error ->
            None

  let rec oasis_status t k =
    oasis_get t k
    >|= 
      function
        | `Set _ -> 
            `OK
        | `Not_found | `Error as e ->
            e
end

(* See ODBStorage.mli *)
let create ~ctxt fs log prev_log_event =
  let res =
    {
      stor_fs   = fs;
      stor_all  = HLS.create ();
      stor_ctxt = ctxt;
    }
  in

  let module SetMergeLog = 
    Set.Make
      (struct
         type t = CalendarLib.Calendar.t * ODBLog.event

         let compare (_, ev1) (_, ev2) = 
           Pervasives.compare ev1 ev2
       end)
  in

  (* Merge two log event lists, used to synchronize logs returned by package and
   * package_version addition with the content of the DB.  Return a set of
   * elements that are new in the first lst.
   *)
  let merge_log lst lst' = 
    let to_set = 
      List.fold_left 
        (fun st e -> SetMergeLog.add e st) 
        SetMergeLog.empty
    in
    let st  = to_set lst in
    let st' = to_set lst' in
      SetMergeLog.diff st st'
  in

  let add_versions pkg_str dn (min_date, acc) =
    FS.fold_dir
      (fun fn bn (min_date, acc) ->
         fs#is_directory fn
         >>= fun is_dir ->
           if is_dir then
             (* Maybe a version *)
             PkgVer.add ~ctxt res pkg_str fn 
             >|= fun (date, ev, pkg_ver) ->
             (min_calendar min_date date, 
              (date, ev) :: acc)
           else
             return (min_date, acc))
      fs dn (min_date, acc)
  in

  let add_packages dn acc = 
    FS.fold_dir 
      (fun fn bn acc ->
         fs#is_directory fn
         >>= fun is_dir ->
           if is_dir then
             (* Maybe a package *)
             Pkg.add ~ctxt res fn
             >>= fun (date, ev, {ODBPkg.pkg_name = pkg_str}) ->
             add_versions pkg_str fn (date, acc)
             >>= fun (min_date, acc) ->
             return ((min_date, ev) :: acc)
           else
             return acc)
      fs dn acc
  in

    add_packages "" []
    >>= fun new_evs ->
    (* Only log real new events *)
    begin
      let st_new = 
        merge_log new_evs 
          (List.rev_map 
             (fun log ->
                log.ODBLog.log_timestamp,
                log.ODBLog.log_event)
             prev_log_event)
      in
      let tasks =
        SetMergeLog.fold 
          (fun (timestamp, ev) acc ->
             (log ~timestamp ev) :: acc)
          st_new
          []
      in
        Lwt.join tasks
    end
    >>= fun () ->
    log 
      ~timestamp:(CalendarLib.Calendar.now ())
      (`Sys 
         (Printf.sprintf "ODBStorage(%s)" fs#root, 
          `Started))
    >>= fun () ->
    return res

(* See ODBStorage.mli *)
let fs t = t.stor_fs

(* See ODBStorage.mli *)
let to_ro t = 
  {t with stor_fs = (t.stor_fs :> FS.std_ro)}
