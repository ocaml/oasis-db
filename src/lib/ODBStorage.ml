
open ODBMessage  
open ODBGettext
open ODBTypes
open ODBInotify
open ODBContext
open ODBUtils
open Lwt

module FS = ODBVFS

type watch = CalendarLib.Calendar.t -> ODBLog.event -> unit Lwt.t

type oasis_content = 
    [ `Set of OASISTypes.package 
    | `Not_found 
    | `Error
    ]

type ver_str = string
type pkg_str = string
type filename = string
type dirname = string

(** Main datastructure *)
type 'a t = 
    {
      stor_fs:          'a;
      stor_ctxt:        ODBContext.t;
      stor_watchers:    watch list; 
      stor_cache_oasis: (filename, oasis_content) Hashtbl.t;
    } constraint 'a = #ODBVFS.read_only

type 'a read_only = (#FS.read_only as 'a) t
type 'a read_write = (#FS.read_write as 'a) t

exception FileDoesntExist 

(** Notify watchers *)
let watch_notify t timestamp ev = 
  Lwt_list.iter_s 
    (fun watcher ->
       watcher timestamp ev)
    t.stor_watchers

let watch_notify_msg t lvl fmt =
  Printf.ksprintf
    (fun s ->
       watch_notify t 
         (CalendarLib.Calendar.now ())
         (`Sys 
            (Printf.sprintf "ODBStorage(%s)" t.stor_fs#id, 
             `Message (lvl, s))))
    fmt

(* Name of the file handling ODBStorage data for a Pkg or a PkgVer *)
let storage_filename dn =
  Filename.concat dn "storage.sexp"

let explode_filename fn =
  let rec explode' acc fn =
    if FilePath.is_current fn then
      acc
    else
      explode' 
        (FilePath.basename fn :: acc) 
        (FilePath.dirname fn)
  in
    explode' [] fn

module type OPS =
sig
  type key 
  type file_type
  type data 

  val filename: 'a t -> key -> file_type -> data -> filename
  val dirname:  'a t -> key -> dirname 
  val from_chn: ctxt:ODBContext.t -> fn:filename -> Lwt_io.input_channel -> data Lwt.t
end

module Common (O: OPS) =
struct
  type key = O.key 
  type file_type = O.file_type 
  type data = O.data

  let dirname = O.dirname 

  let load t k = 
    let fn = storage_filename (dirname t k) in
      ODBVFS.with_file_in t.stor_fs fn
        (O.from_chn ~ctxt:t.stor_ctxt ~fn)

  let filename t k ft = 
    load t k 
    >|= fun data ->
    O.filename t k ft data

  let mem t k =
    catch
      (fun () ->
         load t k
         >|= fun (data : O.data) ->
         true)
      (fun _ ->
         return false)

  let find t k =
    catch 
      (fun () -> 
         load t k)
      (fun e ->
         fail Not_found)

  let with_file_out t k ft f =
    filename t k ft 
    >>= fun fn ->
    ODBVFS.with_file_out t.stor_fs fn f

  let with_file_in ?(catch=(fun e -> fail e)) t k fn read =
    filename t k fn 
    >>= fun fn ->
    t.stor_fs#file_exists fn
    >>= fun exists ->
    if exists then
      Lwt.catch 
        (fun () ->
           ODBVFS.with_file_in t.stor_fs fn read)
        catch
    else
      catch FileDoesntExist

  let file_exists t k ft =
    filename t k ft 
    >>= fun fn ->
    t.stor_fs#file_exists fn
end

module Pkg = 
struct
  open ODBPkg 

  module Common = 
    Common
      (struct
         type file_type = 
             [ `Other of string 
             | `PluginData of string]

         type key =
             [ `Str of pkg_str
             | `Pkg of t
             | `PkgVer of ODBPkgVer.t]

         type data = ODBPkg.t

         let dirname t =
           function
             | `Str s -> s
             | `Pkg t -> t.pkg_name
             | `PkgVer pkg_ver -> pkg_ver.ODBPkgVer.pkg

         let filename t k ft _ =
           let fdn fn = FilePath.concat (dirname t k) fn in
             match ft with
               | `Other fn -> fdn fn
               | `PluginData fn -> fdn (fn^".sexp")

         let from_chn ~ctxt ~fn chn = 
           ODBPkg.from_chn ~ctxt ~fn chn
       end)

  include Common

  (* See ODBStorage.mli *)
  let elements t = 
    catch 
      (fun () ->
         t.stor_fs#readdir ""
         >>= fun arr ->
         Lwt_list.fold_left_s
           (fun acc pkg_str ->
              catch 
                (fun () ->
                   load t (`Str pkg_str)
                   >>= fun pkg ->
                   if pkg.pkg_name = pkg_str then
                     return (pkg :: acc)
                   else
                     watch_notify_msg t 
                       `Error
                       (f_ "Storage file '%s' matches packages '%s' \
                            but is in directory '%s'")
                       (storage_filename (dirname t (`Str pkg_str)))
                       pkg.pkg_name 
                       pkg_str
                     >|= fun () ->
                     acc)
                (fun _ ->
                   return acc))
           []
           (Array.to_list arr))
      (function
         | Unix.Unix_error (Unix.ENOENT, _, _) 
         | Sys_error _ ->
             fail Not_found
         | e ->
             fail e)

  (* See ODBStorage.mli *)
  let create (t: 'a read_write) pkg = 
    let dn = dirname t (`Str pkg.pkg_name) in
      t.stor_fs#mkdir dn 0o755
      >>= fun () ->
      catch 
        (fun () ->
           begin
             let fn = storage_filename dn in
               FS.with_file_out 
                 t.stor_fs fn
                 (ODBPkg.to_chn ~ctxt:t.stor_ctxt ~fn pkg)
               >>= fun () ->
               load t (`Pkg pkg)
           end)
        (fun e ->
           t.stor_fs#rm ~recurse:true [dn]
           >>= fun () ->
           fail e)
end

module PkgVer = 
struct 

  open ODBPkgVer

  module Common = 
    Common
      (struct
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

          type data = ODBPkgVer.t


          let key_str =
            function
              | `Str (_, ver_str) -> 
                  ver_str
              | `StrVer (_, ver) ->
                  OASISVersion.string_of_version ver
              | `PkgVer pkg_ver -> 
                  OASISVersion.string_of_version pkg_ver.ver

          let dirname t k = 
            let pkg_str, dn2 =
              match k with 
                | `Str (pkg_str, ver_str) -> 
                    pkg_str, ver_str
                | `StrVer (pkg_str, ver) ->
                    pkg_str, 
                    OASISVersion.string_of_version ver
                | `PkgVer pkg_ver ->
                    pkg_ver.pkg,
                    OASISVersion.string_of_version pkg_ver.ver 
            in
              FilePath.concat (Pkg.dirname t (`Str pkg_str)) dn2

          let from_chn ~ctxt ~fn chn = 
            ODBPkgVer.from_chn ~ctxt ~fn chn

          let filename t k ft pkg_ver = 
            let fdn fn = FilePath.concat (dirname t k) fn in
              match ft with 
                | `OASIS -> 
                    fdn "_oasis"
                | `OASISPristine -> 
                    fdn "_oasis.pristine"
                | `PluginData nm -> 
                    fdn (nm^".sexp")
                | `Other fn ->
                    fdn fn
                | `Tarball -> 
                    FilePath.concat (dirname t k) pkg_ver.tarball
       end)

  include Common

  (* See ODBStorage.mli *)
  let replace t k pkg_ver = 
    let dn  = dirname t k in
    let dn' = dirname t (`PkgVer pkg_ver) in
      if dn <> dn' then
        fail
          (Failure
             (Printf.sprintf 
                (f_ "Unable to replace '%s' by '%s'")
                dn dn'))
      else
        let fn = storage_filename dn in
          ODBVFS.with_file_out t.stor_fs fn
            (ODBPkgVer.to_chn ~ctxt:t.stor_ctxt ~fn pkg_ver)

  (* See ODBStorage.mli *)
  let elements t pkg_k = 
    let dn = Pkg.dirname t pkg_k in
    let pkg_str = 
      match pkg_k with 
        | `Str pkg_str 
        | `Pkg {ODBPkg.pkg_name = pkg_str}
        | `PkgVer {pkg = pkg_str} ->
            pkg_str
    in
      catch 
        (fun () -> 
           t.stor_fs#readdir dn
           >>= fun arr ->
           Lwt_list.fold_left_s
             (fun acc ver_str ->
                catch 
                  (fun () -> 
                     let k = `Str (pkg_str, ver_str) in
                     let dn = dirname t k in
                       t.stor_fs#is_directory dn 
                       >>= fun is_dir ->
                       begin
                         if is_dir then
                           begin
                             load t k
                             >|= fun pkg_ver ->
                             pkg_ver :: acc
                           end
                         else
                           begin
                             return acc
                           end
                       end)
                  (fun _ ->
                     return acc))
             []
             (Array.to_list arr)
           >|= fun lst ->
           List.sort ODBPkgVer.compare lst)
        (function
           | Unix.Unix_error (Unix.ENOENT, _, _) 
           | Sys_error _ ->
               fail Not_found
           | e ->
               fail e)
    
  (* See ODBStorage.mli *)
  let latest t pkg_k = 
    elements t pkg_k 
    >>= fun lst ->
      match List.rev lst with 
        | hd :: tl ->
            return hd
        | [] -> 
            fail Not_found

  (* See ODBStorage.mli *)
  let create (t: 'a read_write) pkg_ver tarball_chn = 
    Pkg.mem t (`PkgVer pkg_ver)
    >>= fun pkg_exists ->
    begin
      if pkg_exists then
        return ()
      else
        fail
          (Failure 
             (Printf.sprintf 
                (f_ "Package '%s' doesn't exist") 
                pkg_ver.ODBPkgVer.pkg))
    end
    >>= fun () ->
    begin
      let dn = dirname t (`PkgVer pkg_ver) in
        t.stor_fs#mkdir dn 0o755 
        >>= fun () ->
        catch 
          (fun () ->
             (* Copy the tarball *)
             begin
               let fn = Filename.concat dn pkg_ver.ODBPkgVer.tarball in
                 FS.with_file_out
                   t.stor_fs fn 
                   (fun chn_out ->
                      Lwt_io.write_chars chn_out 
                        (Lwt_io.read_chars tarball_chn))
             end
             >>= fun () ->

             (* Create storage.sexp *)
             begin
               let fn = storage_filename dn in
                 FS.with_file_out 
                   t.stor_fs fn
                   (ODBPkgVer.to_chn ~ctxt:t.stor_ctxt ~fn pkg_ver)
             end
             >>= fun () ->

             load t (`PkgVer pkg_ver))

          (fun e ->
             t.stor_fs#rm ~recurse:true [dn]
             >>= fun _ ->
             fail e)
    end

  let oasis_load t k =
      filename t k `OASIS 
      >>= fun fn ->
      catch 
        (fun () -> 
           ODBVFS.with_file_in t.stor_fs fn
             (fun chn ->
                ODBOASIS.from_chn 
                  ~ctxt:t.stor_ctxt 
                  ~fn:(t.stor_fs#vroot fn) chn
                >|= fun oasis ->
                `Set oasis))
        (fun _ ->
           t.stor_fs#file_exists fn
           >|= fun exists ->
             if exists then
               `Error
             else
               `Not_found)
      >|= fun oasis_set ->
      begin
        Hashtbl.replace t.stor_cache_oasis fn oasis_set;
        oasis_set
      end

  let oasis_get t k =
    filename t k `OASIS
    >>= fun fn ->
    try 
      return (Hashtbl.find t.stor_cache_oasis fn)
    with Not_found ->
      oasis_load t k

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

(* Generate events for the watchers *)
let fs_watcher t fn ev = 
  catch 
    (fun () ->
       t.stor_fs#stat fn 
       >|= fun st ->
       CalendarLib.Calendar.from_unixfloat st.Unix.LargeFile.st_mtime )
    (fun _ ->
       return (CalendarLib.Calendar.now ()))
  >>= fun fn_time ->
  match explode_filename fn with 
    | [pkg_str; "storage.sexp"] ->
        begin
          match ev with 
            | ODBVFS.FSCreated ->
                watch_notify t fn_time (`Pkg (pkg_str, `Created))
            | ODBVFS.FSDeleted ->
                watch_notify t fn_time (`Pkg (pkg_str, `Deleted))
            | ODBVFS.FSChanged 
            | ODBVFS.FSMovedTo _ 
            | ODBVFS.FSCopiedFrom _ ->
                return ()
        end
    | [pkg_str; ver_str; "storage.sexp"] ->
        begin
          let ver = OASISVersion.version_of_string ver_str in
            match ev with 
              | ODBVFS.FSCreated ->
                  watch_notify t fn_time (`Pkg (pkg_str, `VersionCreated ver))
              | ODBVFS.FSDeleted ->
                  watch_notify t fn_time (`Pkg (pkg_str, `VersionDeleted ver))
              | ODBVFS.FSChanged 
              | ODBVFS.FSMovedTo _ 
              | ODBVFS.FSCopiedFrom _ ->
                  return ()
        end
    | _ ->
        if FilePath.basename fn = "_oasis" then
          return (Hashtbl.remove t.stor_cache_oasis fn)
        else
          return ()

let create ~ctxt ?(watchers=[]) fs  =
  let res =
    {
      stor_fs          = fs;
      stor_ctxt        = ctxt;
      stor_watchers    = watchers;
      stor_cache_oasis = Hashtbl.create 13;
    }
  in
    watch_notify res
      (CalendarLib.Calendar.now ())
      (`Sys 
         (Printf.sprintf "ODBStorage(%s)" fs#id, 
          `Started))
    >>= fun () ->
    return res

(* See ODBStorage.mli *)
let create_read_only ~ctxt fs =
  create ~ctxt fs 

(* See ODBStorage.mli *)
let create_read_write ~ctxt ?watchers fs  =
  create ~ctxt ?watchers fs
  >|= fun res ->
  begin
    let _i : int =
      res.stor_fs#watch_add (fs_watcher res)
    in
      res
  end

(* See ODBStorage.mli *)
let scan t =
  FS.fold 
    (fun efn () ->
       match efn with 
         | `PostDir _ | `PreDir _ ->
             return ()

         | `File fn ->
             fs_watcher t fn ODBVFS.FSCreated)
    t.stor_fs "" () 

(* See ODBStorage.mli *)
let fs t = t.stor_fs

(* See ODBStorage.mli *)
let to_ro (t: 'a read_write) = 
  {t with stor_fs = (t.stor_fs :> FS.read_only)}
