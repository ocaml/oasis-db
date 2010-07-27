
open ODBMessage
open ODBGettext
open Lwt

module MapString = Map.Make(String)

type t = 
  {
    pkg:  ODBPkg.t;
    vers: ODBVer.t MapString.t
  }

(** All packages reference *)
let rpackages = 
  ref MapString.empty 

(** Protect all packages reference *)
let rpackages_lock =
  Lwt_mutex.create () 

(** Get all packages *)
let packages () = 
  Lwt_mutex.with_lock 
    rpackages_lock
    (fun () -> return !rpackages)

(** Get the directory of a package
 *)
let dirname_of_package pkg =
  Filename.concat ODBConf.dist_dir pkg 

(** Get the directory of a package's version
 *)
let dirname_of_version ver =
  Filename.concat 
    (dirname_of_package ver.ODBVer.pkg) 
    (OASISVersion.string_of_version ver.ODBVer.ver)

let check ~ctxt () = 
  (* TODO: check that ver.pkg = pkg in the packages
   * data structure 
   *)
  (* TODO: check that reloading data is possible 
   * and lead to the same result.
   *)
  ()

let init ~ctxt () = 
  let with_storage_sexp parse f full nm acc = 
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

  let check_list lst = 
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

  let fold_versions pkg_str dn vers =
    ODBFileUtil.fold_dir 
      (with_storage_sexp 
        ODBVer.from_file
        (fun storage_fn ver _ nm vers ->
          let ver_str = 
            OASISVersion.string_of_version ver.ODBVer.ver
          in
            check_list
            [
              nm <> ver_str,
              false,
              spf (f_ "Storage file '%s' is version '%s' but is contained \
                       in directory '%s'") storage_fn ver_str nm;

              MapString.mem ver_str vers,
              true,
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
                 MapString.add ver_str ver vers
               else
                 vers)))
      dn
      vers
  in

  let fold_packages dn pkgs =
    ODBFileUtil.fold_dir 
      (with_storage_sexp 
        ODBPkg.from_file
        (fun storage_fn pkg full nm pkgs ->
          let pkg_str = 
            pkg
          in
            check_list
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
                fold_versions pkg_str full MapString.empty 
                >>= fun vers ->
                begin
                  let t = 
                    {
                      pkg  = pkg;
                      vers = vers;
                    }
                  in
                    return (MapString.add pkg_str t pkgs)
                end
              end
            else
              begin
                return pkgs
              end))
      dn 
      pkgs
  in

    (* Search packages and their versions *)
    fold_packages
      ODBConf.dist_dir
      MapString.empty
    >>= fun mp ->
    (* Set global variable *)
    Lwt_mutex.with_lock 
      rpackages_lock
      (fun () -> 
        rpackages := mp;
        return ())


(** Fold over all available packages
  *)
let fold_packages f acc = 
  packages () 
  >>= fun mp ->
  return 
    (MapString.fold
      (fun _ t acc -> f t.pkg acc)
      mp
      acc)

(** Fold over all available version of a package
  *)
let fold_versions pkg f acc = 
  packages () 
  >>= fun mp ->
  return 
    (try 
       let pkg = 
         MapString.find pkg mp
       in
         MapString.fold
           (fun _ t acc -> f t acc)
           pkg.vers
           acc
     with e ->
       fail e)

(** Check the existence of a package
 *)
let package_exists pkg = 
  packages () 
  >>= fun pkgs ->
  return (MapString.mem pkg pkgs)

(** Check the existence of a package's version
 *)
let version_exists pkg ver = 
  packages () 
  >>= fun pkgs ->
  begin
    try
      let p = 
        MapString.find pkg pkgs
      in
        return (MapString.mem ver p.vers)
    with Not_found ->
      return false
  end

