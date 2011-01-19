
open ODBMessage
open ODBGettext
open ODBTypes
open ODBInotify
open ODBContext
open ODBUtils
open Lwt

(* TODO: using Lwt for almost every function seem overkilling, try to 
 * use something more lightweight (e.g. don't use Lwt except for blocking
 * call)
 *)

(** A container that mix a Hashtbl with a way to fast 
    enumerate elements (based on Queue)
  *)
module HLS = 
struct

  let key_equal = 
    ( = ) 

  module HashString = 
    Hashtbl.Make
      (struct
         type t = string
         let equal = key_equal
         let hash = Hashtbl.hash
       end)

  type 'a t = 
      {
        tbl: 'a HashString.t;
        que: string Queue.t;
      }

  let create () = 
    {
      tbl = HashString.create 13;
      que = Queue.create ();
    }

  (* TODO: considering locking/notification *)

  let add t k v = 
    if not (HashString.mem t.tbl k) then
      Queue.add k t.que;
    HashString.replace t.tbl k v;
    return ()

  let find t k =
    try 
      return (HashString.find t.tbl k)
    with e ->
      fail e 

  let mem t k =
    return (HashString.mem t.tbl k)

  let elements t = 
    let lst = 
      Queue.fold 
        (fun acc k ->
           (k, HashString.find t.tbl k) :: acc)
        []
        t.que
    in
      return lst

end

(** Storage information for a version 
  *)
type ver_t =
  {
    ver: ODBPkgVer.t;
    ver_dir: dirname;
  }

(** Storage information for a package
  *)
type pkg_t = 
  {
    pkg:  ODBPkg.t;
    pkg_dir:  dirname;
    vers: ver_t HLS.t
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

  (** All packages reference *)
  let all = 
    HLS.create ()

  (** All available packages
    *)
  let elements () = 
    HLS.elements all
    >|= 
    List.map (fun (_, t) -> t.pkg)

  (** Check the existence of a package
   *)
  let mem k = 
    HLS.mem all k

  (** Get a specific package
    *)
  let find k =
    HLS.find all k
    >|= 
    fun t -> t.pkg

  (* Add a package that is already in the dist_dir *)
  let add ~ctxt dn =  
    let storage_fn = 
      storage_filename dn
    in
      ODBPkg.from_file ~ctxt storage_fn
      >>= fun (pkg as pkg_str) ->
      HLS.mem all pkg_str 
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
              FilePath.is_subdir ctxt.dist_dir dn,
              false,
              spf (f_ "Directory '%s' is not a subdirectory of dist dir '%s'")
                dn ctxt.dist_dir;

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
      begin
        let t = 
          {
            pkg     = pkg;
            pkg_dir = dn;
            vers    = HLS.create ();
          }
        in
          HLS.add all pkg_str t
          >>= fun () ->
          return t
      end
      >>= fun t ->
      info ~ctxt (f_ "New package %s") pkg_str
      >>= fun () ->
      return t.pkg

  (** Move a package define outside the dist_dir, into it and add it *)
  let move ~ctxt dn =
    let storage_fn = 
      storage_filename dn
    in
      ODBPkg.from_file ~ctxt storage_fn 
      >>= fun (pkg as pkg_str) ->
      begin
        let tgt_dn = 
          Filename.concat ctxt.dist_dir pkg_str
        in
          try 
            Sys.rename dn tgt_dn;
            return tgt_dn
          with e ->
            fail e
      end 
      >>= fun tgt_dn ->
      catch
        (fun () -> 
           add ~ctxt tgt_dn)
        (fun e ->
           (* We have a problem, restore directory *)
           begin
             try 
               Sys.rename tgt_dn dn;
               return ()
             with e ->
               error ~ctxt "%s" (string_of_exception e)
           end
           >>= fun () ->
           fail e)

  (** Create a package directory outside the dist_dir *)
  let create ~ctxt pkg = 
    ODBFileUtil.temp_dir ~ctxt "oasis-db" "-pre-pkg.dir"
    >>= fun dn ->
    ODBPkg.to_file ~ctxt (storage_filename dn) pkg
    >>= fun () ->
    return dn 

  let dirname k = 
    HLS.find all k
    >>= fun t ->
    return t.pkg_dir

  let filename k fn =
    dirname k 
    >|= fun dn ->
    begin
      match fn with 
        | `PluginData plg ->
            Filename.concat dn (plg^".sexp")
    end
end

module Ver = 
struct 
  (** Get the [ver_t HLS.t] out of [Pkg.all].
    *)
  let get pkg_str = 
    HLS.find Pkg.all pkg_str
    >>= fun pkg_strg ->
    return pkg_strg.vers

  (** All available version of a package, beginning with the older
      one.
    *)
  let elements pkg_str = 
    get pkg_str
    >>= 
    HLS.elements 
    >|= 
    List.rev_map (fun (_, t) -> t.ver) 
    >|=
    List.sort ODBPkgVer.compare
    
  (** Check the existence of a package's version
   *)
  let mem pkg_str k = 
    get pkg_str
    >>= fun vers ->
    HLS.mem vers k

  (** Get a specific version
    *)
  let find pkg_str k =
    get pkg_str 
    >>= fun vers ->
    HLS.find vers k
    >|= (fun t -> t.ver)

  (** Get the latest version
    *)
  let latest pkg_str = 
    elements pkg_str 
    >|= List.rev
    >>= function
      | [] -> 
          fail Not_found
      | e :: _ ->
          return e

  (** Add a version that is already in the dist_dir *)
  let add ~ctxt pkg_str dn = 
    let storage_fn = 
      storage_filename dn 
    in
      ODBPkgVer.from_file ~ctxt storage_fn 
      >>= fun ver ->
      return (OASISVersion.string_of_version ver.ODBPkgVer.ver)
      >>= fun ver_str ->
      get pkg_str 
      >>= fun vers ->
      HLS.mem vers ver_str
      >>= fun ver_exists ->
      Pkg.dirname pkg_str 
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

              ver.ODBPkgVer.pkg <> pkg_str,
              true, 
              spf (f_ "Storage file '%s' belongs to package '%s' but \
                       is stored in package '%s'.")
                storage_fn ver.ODBPkgVer.pkg pkg_str;
            ]
      end
      >>= fun () ->
      begin
        let t = 
          {
            ver     = ver;
            ver_dir = dn;
          }
        in
          HLS.add vers ver_str t
          >>= fun () ->
          return t 
      end 
      >>= fun t ->
      info ~ctxt (f_ "New version %s/%s") pkg_str ver_str
      >>= fun () ->
      return t.ver

  (** Move a version created outside the dist_dir, inside it and add it *)
  let move ~ctxt dn = 
    let storage_fn =
      storage_filename dn 
    in
      ODBPkgVer.from_file ~ctxt storage_fn
      >>= fun ver ->
      catch 
        (fun () -> 
           Pkg.dirname ver.ODBPkgVer.pkg)
        (function
           | Not_found as e -> 
               error ~ctxt (f_ "Package '%s' doesn't exist") ver.ODBPkgVer.pkg
               >>= fun () ->
               fail e
           | e ->
               fail e)
      >>= fun pkg_dn ->
      begin
        let ver_str = OASISVersion.string_of_version ver.ODBPkgVer.ver in
        let tgt_dn  = Filename.concat pkg_dn ver_str in
          try 
            Sys.rename dn tgt_dn; 
            return (ver_str, tgt_dn)
          with e ->
            fail e
      end
      >>= fun (ver_str, tgt_dn) ->
      catch 
        (fun () ->
           add ~ctxt ver.ODBPkgVer.pkg tgt_dn)
        (fun e ->
           (* We have a problem, restore directory *)
           begin
             try 
               Sys.rename tgt_dn dn;
               return ()
             with e ->
               error ~ctxt "%s" (string_of_exception e)
               >>= fun () ->
               fail e
           end
           >>= fun () ->
           fail e)

  (** Initialize a directory containing a version 
      out of the dist_dir
    *)
  let init ~ctxt ver dn = 
    begin
      let oasis_fn = 
        Filename.concat dn "_oasis"
      in
      let oasis_pristine_fn = 
        oasis_fn ^ ".pristine"
      in
        if Sys.file_exists oasis_fn &&
           not (Sys.file_exists oasis_pristine_fn) then
          ODBFileUtil.cp ~ctxt [oasis_fn] oasis_pristine_fn
        else
          return ()
    end
    >>= fun () ->
    ODBPkgVer.to_file ~ctxt (storage_filename dn) ver

  (** Return the directory name of a version 
    *)
  let dirname pkg_str k = 
    get pkg_str
    >>= fun vers ->
    HLS.find vers k
    >>= fun t ->
    return t.ver_dir

  (** Resolve the name of a file for a particular version
    *)
  let filename pkg_str k fn = 
    get pkg_str
    >>= fun vers ->
    HLS.find vers k
    >>= fun t ->
    begin
      let bn =
        match fn with 
          | `OASIS -> "_oasis"
          | `OASISPristine -> "_oasis.pristine"
          | `Tarball -> t.ver.ODBPkgVer.tarball
          | `PluginData nm -> nm^".sexp"
          | `Other fn -> fn
      in
        return (Filename.concat t.ver_dir bn)
    end
end


let init ~ctxt () = 

  let add_versions pkg_str dn =
    ODBFileUtil.fold_dir 
      (fun fn bn () ->
         if Sys.is_directory fn then
           (* Maybe a version *)
           Ver.add ~ctxt pkg_str fn 
           >>= fun _ ->
           return ()
         else
           return ())
      dn ()
  in

  let add_packages dn = 
    ODBFileUtil.fold_dir 
      (fun fn bn () ->
         if Sys.is_directory fn then
           (* Maybe a package *)
           Pkg.add ~ctxt fn
           >>= fun (pkg as pkg_str) ->
           add_versions pkg_str fn
         else
           return ())
      dn ()
  in

    add_packages ctxt.dist_dir 


let check ~ctxt () = 
  (* TODO: check that ver.pkg = pkg in the packages
   * data structure 
   *)
  (* TODO: check that reloading data is possible 
   * and lead to the same result.
   *)
  ()

