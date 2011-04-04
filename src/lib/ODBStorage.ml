
open ODBMessage
open ODBGettext
open ODBTypes
open ODBInotify
open ODBContext
open ODBUtils
open Lwt

module HLS = ODBHLS

(** Storage information for a version 
  *)
type pkg_ver_t =
  {
    pkg_ver: ODBPkgVer.t;
    pkg_ver_dir: dirname;
  }

(** Storage information for a package
  *)
type pkg_t = 
  {
    pkg:      ODBPkg.t;
    pkg_dir:  dirname;
    pkg_vers: pkg_ver_t HLS.t
  }

(** Main datastructure *)
type 'a t = 
    {
      fs:  ODBFilesystem.std;
      all: pkg_t HLS.t;
    }

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

module Pkg = 
struct
  open ODBPkg 

  (** All available packages
    *)
  let elements t = 
    HLS.elements t.all
    >|= 
    List.map (fun (_, pkg_t) -> pkg_t.pkg)

  (** Check the existence of a package
   *)
  let mem t k = 
    HLS.mem t.all k

  (** Get a specific package
    *)
  let find t k =
    HLS.find t.all k
    >|= fun t -> 
    t.pkg

  (* Add a package that is already in the filesystem *)
  let add ~ctxt t dn =  
    let storage_fn = 
      storage_filename dn 
    in
      t.fs#with_file_in
        storage_fn
        (ODBPkg.from_chn ~ctxt ~fn:storage_fn)
      >>= fun ({pkg_name = pkg_str} as pkg) ->
      HLS.mem t.all pkg_str
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
      t.fs#stat storage_fn
      >>= fun stat ->
      begin
        let date =
          CalendarLib.Calendar.from_unixfloat
            stat.Unix.st_mtime
        in
        let pkg_t = 
          {
            pkg      = pkg;
            pkg_dir  = dn;
            pkg_vers = HLS.create ();
          }
        in
          HLS.add t.all pkg_str pkg_t
          >|= fun () ->
          (date, `Pkg (pkg_str, `Created), pkg)
      end

  (** Create a package *)
  let create ~ctxt t pkg = 
    t.fs#mkdir pkg 0o755
    >>= fun () ->
    catch 
      (fun () ->
         begin
           let fn = 
             storage_filename pkg
           in
             t.fs#with_file_out
               fn
               (ODBPkg.to_chn ~ctxt ~fn pkg)
         end
         >>= fun () ->
         add ~ctxt t pkg)
      (fun e ->
         t.fs#rm ~recurse:true [pkg]
         >>= fun () ->
         fail e)
  
  let dirname t k = 
    HLS.find t.all k
    >>= fun t ->
    return t.pkg_dir

  let filename t k fn =
    dirname t k 
    >|= fun dn ->
    begin
      let bn = 
        match fn with 
          | `PluginData plg ->
             plg^".sexp"
          | `Other fn ->
              fn
      in
        Filename.concat dn bn
    end

  let with_file_in t k fn read dflt =
    filename t k fn 
    >>= fun fn ->
    t.fs#file_exists fn
    >>= fun exists ->
    if exists then
      t.fs#with_file_in fn read
    else
      dflt ()
end

module PkgVer = 
struct 
  (** Get the [pkg_ver_t HLS.t] out of [Pkg.all].
    *)
  let get t pkg_str = 
    HLS.find t.all pkg_str
    >>= fun pkg_strg ->
    return pkg_strg.pkg_vers

  (** All available version of a package, beginning with the older
      one.
    *)
  let elements ?extra t pkg_str = 
    get t pkg_str
    >>= 
    HLS.elements
    >|= 
    List.rev_map (fun (_, t) -> t.pkg_ver) 
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
    
  (** Check the existence of a package's version
   *)
  let mem t pkg_str k = 
    get t pkg_str
    >>= fun pkg_vers ->
    HLS.mem pkg_vers k

  (** Get a specific version
    *)
  let find t pkg_str k =
    get t pkg_str 
    >>= fun pkg_vers ->
    HLS.find pkg_vers k
    >|= (fun t -> t.pkg_ver)

  (** Get the latest version
    *)
  let latest ?extra t pkg_str = 
    elements ?extra t pkg_str 
    >|= List.rev
    >>= function
      | [] -> 
          fail Not_found
      | e :: _ ->
          return e

  (** Add a package version that is already in the dist_dir *)
  let add ~ctxt t pkg_str dn = 
    let storage_fn = 
      storage_filename dn 
    in
      t.fs#with_file_in storage_fn 
        (ODBPkgVer.from_chn ~ctxt ~fn:storage_fn)
      >>= fun pkg_ver ->
      return (OASISVersion.string_of_version pkg_ver.ODBPkgVer.ver)
      >>= fun ver_str ->
      get t pkg_str 
      >>= fun pkg_vers ->
      HLS.mem pkg_vers ver_str
      >>= fun ver_exists ->
      Pkg.dirname t pkg_str 
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
        let t = 
          {
            pkg_ver     = pkg_ver;
            pkg_ver_dir = dn;
          }
        in
          HLS.add pkg_vers ver_str t
          >|= fun () ->
          (pkg_ver.ODBPkgVer.upload_date,
           `Pkg (pkg_ver.ODBPkgVer.pkg, 
                 `VersionCreated pkg_ver.ODBPkgVer.ver),
           pkg_ver)
      end

  (** Create a package version 
    *)
  let create ~ctxt t pkg_ver tarball_fd = 
    catch 
      (fun () -> 
         Pkg.dirname t pkg_ver.ODBPkgVer.pkg)
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
        t.fs#mkdir dn 0o755 
        >>= fun () ->
        catch 
          (fun () ->
             (* Create storage.sexp *)
             begin
               let fn =  storage_filename dn in
                 t.fs#with_file_out
                   fn
                   (ODBPkgVer.to_chn ~ctxt ~fn pkg_ver)
             end
             >>= fun () ->

             (* Copy the tarball *)
             t.fs#copy_fd
               tarball_fd 
               (Filename.concat dn pkg_ver.ODBPkgVer.tarball)
             >>= fun () ->

             (* Notify installation of a new package version *)
             add ~ctxt t pkg_ver.ODBPkgVer.pkg dn)

          (fun e ->
             t.fs#rm ~recurse:true [dn]
             >>= fun _ ->
             fail e)
    end

  (** Return the directory name of a version 
    *)
  let dirname t pkg_str k = 
    get t pkg_str
    >>= fun pkg_vers ->
    HLS.find pkg_vers k
    >>= fun t ->
    return t.pkg_ver_dir

  (** Resolve the name of a file for a particular version
    *)
  let filename t pkg_str k fn = 
    get t pkg_str
    >>= fun pkg_vers ->
    HLS.find pkg_vers k
    >>= fun t ->
    begin
      let bn =
        match fn with 
          | `OASIS -> "_oasis"
          | `OASISPristine -> "_oasis.pristine"
          | `Tarball -> t.pkg_ver.ODBPkgVer.tarball
          | `PluginData nm -> nm^".sexp"
          | `Other fn -> fn
      in
        return (Filename.concat t.pkg_ver_dir bn)
    end

  let with_file_out t pkg_str k fn f =
    filename t pkg_str k fn 
    >>= fun fn ->
    t.fs#with_file_out fn f 

  let with_file_in t pkg_str k fn f = 
    filename t pkg_str k fn
    >>= fun fn ->
    t.fs#with_file_in fn f
end

(* Create the datastructure, using the content of the filesystem *)
let create ~ctxt fs log prev_log_event =
  let res =
    {
      fs  = fs;
      all = HLS.create ();
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
    fs#fold_dir 
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
      dn (min_date, acc)
  in

  let add_packages dn acc = 
    fs#fold_dir 
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
      dn acc
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

let create_ro =
  create

let create_rw =
  create

