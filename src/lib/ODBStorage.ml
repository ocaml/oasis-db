
open ODBMessage
open ODBGettext
open ODBTypes
open ODBInotify
open ODBContext
open Lwt

module MapString = Map.Make(String)

type ver_t =
  {
    ver: ODBVer.t;
    ver_dir: dirname;
  }

type pkg_t = 
  {
    pkg:  ODBPkg.t;
    pkg_dir:  dirname;
    vers: ver_t MapString.t
  }

(** All packages reference *)
let rpackage_tree = 
  ref MapString.empty 

(** Protect all packages reference *)
let rpackage_tree_lock =
  Lwt_mutex.create () 

(** Notify about changes in the tree *)
let rpackage_tree_changed =
  Lwt_condition.create ()

(** Get all packages *)
let package_tree_get () = 
  Lwt_mutex.with_lock 
    rpackage_tree_lock
    (fun () -> return !rpackage_tree)

(** Modify package tree, using a lock *)
let package_tree_mod f = 
  Lwt_mutex.with_lock 
    rpackage_tree_lock
    (fun () -> 
      f !rpackage_tree
      >>= fun ptree ->
      rpackage_tree := ptree;
      Lwt_condition.broadcast rpackage_tree_changed ();
      return ())

(** [package_tree_changed ~timeout cond] Wait that the [cond] become [true]
    and return [true] or that [timeout] seconds has passed and return
    [false]
  *)
let package_tree_changed ?(timeout=1.0) cond =
  Lwt_mutex.with_lock
    rpackage_tree_lock
    (fun () ->

      let rec package_tree_changed_aux () = 
        cond !rpackage_tree
        >>= function
          | true ->
              return true

          | false ->
              (* Wait for a change in the tree *)
              Lwt_condition.wait 
                ~mutex:rpackage_tree_lock 
                rpackage_tree_changed
              >>= 
              package_tree_changed_aux
      in

      let timeout_run () = 
        Lwt_unix.sleep timeout
        >>= fun () ->
        return false
      in

      Lwt.choose [package_tree_changed_aux (); timeout_run ()])

(* TODO: use this somewhere
(** Get the directory of a package
 *)
let dirname_of_package pkg =
  Filename.concat ctxt.dist_dir pkg 

(** Get the directory of a package's version
 *)
let dirname_of_version ver =
  Filename.concat 
    (dirname_of_package ver.ODBVer.pkg) 
    (OASISVersion.string_of_version ver.ODBVer.ver)
    *)

let check ~ctxt () = 
  (* TODO: check that ver.pkg = pkg in the packages
   * data structure 
   *)
  (* TODO: check that reloading data is possible 
   * and lead to the same result.
   *)
  ()

let run = 
  let with_storage_sexp ~ctxt parse f full nm acc = 
    let fn = 
      Filename.concat full "storage.sexp" 
    in
      if Sys.file_exists fn then
        begin
          parse ~ctxt fn 
          >>= fun t ->
          f fn t full nm acc
        end
      else
        begin
          warning ~ctxt
            (f_ "Storage file '%s' doesn't exist")
            fn
          >>= fun () ->
          return acc
        end
  in

  let check_list ~ctxt lst = 
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
      return proceed
  in

  let spf fmt =
    Printf.sprintf fmt
  in

  let init_version ~ctxt pkg_str dn vers = 
    with_storage_sexp ~ctxt 
     ODBVer.from_file
     (fun storage_fn ver full nm vers ->
       let ver_str = 
         OASISVersion.string_of_version ver.ODBVer.ver
       in
         check_list ~ctxt 
         [
           nm <> ver_str,
           false,
           spf (f_ "Storage file '%s' is version '%s' but is contained \
                    in directory '%s'") storage_fn ver_str nm;

           (* TODO: don't warn if storage_fn = previous storage_fn *)
           MapString.mem ver_str vers,
           false,
           spf (f_ "Storage file '%s' defines version '%s' previously \
                    defined.") storage_fn ver_str;

           ver.ODBVer.pkg <> pkg_str,
           true, 
           spf (f_ "Storage file '%s' belongs to package '%s' but \
                    is stored in package '%s'.")
             storage_fn ver.ODBVer.pkg pkg_str;
         ]
         >>= fun proceed ->
         return 
           (if proceed then 
             begin
               let ver_t = 
                 {
                   ver     = ver;
                   ver_dir = full;
                 }
               in
                 MapString.add ver_str ver_t vers
             end
            else
              vers))
     dn vers
  in

  let fold_versions ~ctxt pkg_str dn vers =
    ODBFileUtil.fold_dir (init_version ~ctxt pkg_str) dn vers
  in

  let init_package ~ctxt fn nm acc = 
    with_storage_sexp ~ctxt 
     ODBPkg.from_file
     (fun storage_fn pkg full nm pkgs ->
       let pkg_str = 
         pkg
       in
         check_list ~ctxt
         [
            nm <> pkg_str,
            false,
            spf (f_ "Storage file '%s' matches package '%s' but is in \
                     directory '%s'")
              storage_fn pkg_str nm;

            MapString.mem pkg_str pkgs,
            true,
            spf (f_ "Storage file '%s' defines package '%s' previously \
                     defined.") 
              storage_fn pkg_str;
         ]
         >>= fun proceed ->
         if proceed then 
           begin
             fold_versions ~ctxt pkg_str full MapString.empty 
             >>= fun vers ->
             begin
               let t = 
                 {
                   pkg     = pkg;
                   pkg_dir = full;
                   vers    = vers;
                 }
               in
                 return (MapString.add pkg_str t pkgs)
             end
           end
         else
           begin
             return pkgs
           end) 
     fn nm acc
  in

  let fold_packages ~ctxt dn pkgs =
    ODBFileUtil.fold_dir (init_package ~ctxt) dn pkgs
  in

  let dispatch ~ctxt ev () = 
    match ev with 
    | Dir _ -> 
        return ()

    | File (Created _) ->
        return ()

    | File (Changed fn) ->
        begin
          if FilePath.is_subdir fn ctxt.dist_dir then
            begin
              try 
                let pwd = 
                  FileUtil.pwd ()
                in
                let rel_fn = 
                  FilePath.make_relative 
                    (FilePath.make_absolute pwd ctxt.dist_dir)
                    (FilePath.make_absolute pwd fn)
                in
                let rec split_fn fn acc =
                  let bn = FilePath.basename fn in
                  let dn = FilePath.dirname fn in
                    if FilePath.is_current bn then
                      acc
                    else
                      split_fn dn (bn :: acc)
                in

                  match split_fn rel_fn [] with 
                  | pkg :: ver :: "storage.sexp" :: [] ->
                      package_tree_mod 
                        (fun ptree ->
                          let pkg_t = 
                            MapString.find pkg ptree
                          in
                            init_version ~ctxt 
                              pkg 
                              (FilePath.dirname fn) 
                              ver 
                              pkg_t.vers
                            >>= fun vers ->
                            return 
                              (MapString.add 
                                pkg 
                                {pkg_t with vers = vers} 
                                ptree))

                  | pkg :: "storage.sexp" :: [] ->
                      package_tree_mod 
                        (fun ptree ->
                          init_package 
                            ~ctxt 
                            (FilePath.dirname fn) 
                            pkg 
                            ptree)

                  | _ ->
                      debug ~ctxt 
                        (f_ "Don't know what to do with '%s'")
                        fn

              with e ->
                fail e
            end
          else
            fail 
              (Failure 
                (Printf.sprintf
                  (f_ "Monitored file '%s' is not in '%s'")
                  fn ctxt.dist_dir))
        end

    | File (Deleted fn) ->
        (* TODO *)
        return ()
  in

    ODBRunner.singleton
      "ODBStorage.run"
      (fun ~ctxt () ->
        let ctxt = ODBContext.sub ctxt "storage" in
        (* Initialize packages and their versions *)
        package_tree_mod (fold_packages ~ctxt ctxt.dist_dir)
        >>= fun () ->
        monitor_fs ~ctxt (dispatch ~ctxt) ctxt.dist_dir ())


(** All available packages
  *)
let packages () = 
  package_tree_get () 
  >>= fun ptree ->
  return 
    (MapString.fold
      (fun _ t acc -> t.pkg :: acc)
      ptree
      [])

(** All available version of a package, beginning with the older
    one.
  *)
let versions pkg = 
  package_tree_get () 
  >>= fun ptree ->
  try 
    let pkg = 
      MapString.find pkg ptree 
    in
    let vers =
      (* All versions sorted, starting by the older one *)
      List.sort 
        ODBVer.compare
        (* Get all versions *)
        (MapString.fold
         (fun _ t acc -> t.ver :: acc)
         pkg.vers
         [])
    in
    return vers

  with e ->
    fail e


let package_low pkg = 
  package_tree_get ()
  >>= fun ptree ->
  try 
    return (MapString.find pkg ptree)
  with e ->
    fail e

(** Get a specific package
  *)
let package pkg =
  package_low pkg
  >>= fun pkg_t ->
  return pkg_t.pkg


let version_low pkg ver =
  package_low pkg
  >>= fun pkg_t ->
  try 
    return (MapString.find ver pkg_t.vers)
  with e ->
    fail e

(** Get a specific version
  *)
let version pkg ver =
  version_low pkg ver
  >>= fun ver_t ->
  return ver_t.ver

(** Get the latest version
  *)
let version_latest pkg = 
  versions pkg 
  >>= fun lst ->
  match List.rev lst with 
    | [] ->
        fail Not_found
    | e :: _ ->
        return e

(** Check the existence of a package
 *)
let package_exists pkg = 
  package_tree_get () 
  >>= fun ptree ->
  return (MapString.mem pkg ptree)

type version_file =
  | OASIS 
  | OASISPristine 
  | Tarball
  | PluginData of name
  | OtherFileVersion of filename

let version_filename_low ver_t ver_fn =
  let bn =
    match ver_fn with 
      | OASIS -> "_oasis"
      | OASISPristine -> "_oasis.pristine"
      | Tarball -> ver_t.ver.ODBVer.tarball
      | PluginData nm -> nm^".sexp"
      | OtherFileVersion fn -> fn
  in
    Filename.concat ver_t.ver_dir bn

(** Resolve the name of a file for a particular version
  *)
let version_filename pkg ver ver_fn = 
  version_low pkg ver
  >>= fun ver_t ->
  return (version_filename_low ver_t ver_fn)


(** Check the existence of a package's version
 *)
let version_exists pkg ver = 
  catch 
    (fun () -> 
       version pkg ver
       >>= fun _ ->
       return true)
    (function
       | Not_found -> return false
       | e -> fail e)

exception PackageAlreadyExists of string
exception VersionAlreadyExists of string * string
exception PackageCreationTimeout of string
exception VersionCreationTimeout of string * string

let add_package ~ctxt pkg =
  package_exists pkg
  >>= function 
    | true ->
        fail (PackageAlreadyExists pkg)
    | false ->
        let dn = Filename.concat ctxt.dist_dir pkg in
        ODBFileUtil.mkdir ~ignore_exist:true dn 0o755
        >>= fun () ->
        ODBPkg.to_file ~ctxt 
          (Filename.concat dn "storage.sexp") 
          (ODBPkg.make pkg)
        >>= fun () ->
        (* Wait for package creation *)
        package_tree_changed 
          (fun ptree -> return (MapString.mem pkg ptree))
        >>= 
        begin
          function
            | true ->
                (* TODO: move this to init package *)
                info ~ctxt
                  (f_ "New package %s")
                  pkg 
            | false ->
                fail (PackageCreationTimeout pkg)
        end

let add_version ~ctxt ver tarball_fn oasis_fn = 
  let pkg = ver.ODBVer.pkg in
  let ver_s = OASISVersion.string_of_version ver.ODBVer.ver in
    package_exists pkg 
    >>= 
    begin
      function
        | true ->
            begin
              version_exists pkg ver_s
              >>= function
                | true  -> 
                    fail (PackageAlreadyExists pkg)
                | false -> 
                    return ()
            end

        | false ->
            add_package ~ctxt pkg
    end
    >>= 
    package_tree_get 
    >>= fun ptree ->
    try
      let pkg_t = MapString.find pkg ptree in
      let dn    = Filename.concat pkg_t.pkg_dir ver_s in

        ODBFileUtil.mkdir ~ignore_exist:true dn 0o755
        >>= fun () ->
        ODBFileUtil.cp ~ctxt [tarball_fn] dn
        >>= fun () ->
        begin
          (* TODO: Possible race condition here: oasis_fn points
           * to temporary storage, it could have been deleted
           *)
          match oasis_fn with
            | Some fn -> 
                begin
                  let cp_to = 
                    ODBFileUtil.cp ~ctxt [fn] 
                  in
                    cp_to (Filename.concat dn "_oasis")
                    >>= fun () ->
                    cp_to (Filename.concat dn "_oasis.pristine")
                end
            | None -> 
                return ()
        end
        >>= fun () ->
        ODBVer.to_file ~ctxt 
          (Filename.concat dn "storage.sexp")
          ver
        >>= fun () ->
        (* Wait for version creation *)
        package_tree_changed
          (fun ptree ->
            try 
              let pkg_t =
                MapString.find pkg ptree
              in
                return (MapString.mem ver_s pkg_t.vers)
            with Not_found ->
              return false)
        >>= function
          | true ->
              (* TODO: move this to init version *)
              info ~ctxt
                (f_ "New version %s/%s")
                pkg ver_s
          | false ->
              fail (VersionCreationTimeout (pkg, ver_s))

    with 
      | Not_found ->
          (* Package not found, should not happen... *)
          fail 
            (Failure 
               (Printf.sprintf (f_ "Package '%s' not created as expected") pkg))
      | e ->
          fail e

