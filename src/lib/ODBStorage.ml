
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

  (** Create a package *)
  let create ~ctxt pkg = 
    let dn = 
      Filename.concat ctxt.dist_dir pkg
    in
      ODBFileUtil.mkdir dn 0o755
      >>= fun () ->
      catch 
        (fun () ->
           ODBPkg.to_file ~ctxt (storage_filename dn) pkg
           >>= fun () ->
           add ~ctxt dn)
        (fun e ->
           ODBFileUtil.rm ~ctxt ~recurse:true [dn]
           >>= fun () ->
           fail e)
  
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
  let elements ?extra pkg_str = 
    get pkg_str
    >>= 
    HLS.elements 
    >|= 
    List.rev_map (fun (_, t) -> t.ver) 
    >|= fun lst ->
    begin
      let lst = 
        match extra with 
          | Some ver -> ver :: lst
          | None -> lst
      in
        List.sort ODBPkgVer.compare lst
    end
    
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
  let latest ?extra pkg_str = 
    elements ?extra pkg_str 
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

  (** Create a package version 
    *)
  let create ~ctxt pkg_ver tarball_fd = 
    catch 
      (fun () -> 
         Pkg.dirname pkg_ver.ODBPkgVer.pkg)
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
        ODBFileUtil.mkdir dn 0o755 
        >>= fun () ->
        catch 
          (fun () ->
             (* Create storage.sexp *)
             ODBPkgVer.to_file ~ctxt 
               (storage_filename dn) pkg_ver
             >>= fun () ->

             (* Copy the tarball *)
             begin
               let chn_in = 
                 Lwt_io.of_unix_fd 
                   ~mode:Lwt_io.input 
                   (Unix.dup tarball_fd)
               in
                 finalize 
                   (fun () ->
                      Lwt_io.with_file
                        ~mode:Lwt_io.output
                        (Filename.concat dn pkg_ver.ODBPkgVer.tarball)
                        (fun chn_out ->
                           Lwt_io.write_chars 
                             chn_out
                             (Lwt_io.read_chars chn_in)))
                   (fun () ->
                      Lwt_io.close chn_in)
             end
             >>= fun () ->

             (* Notify installation of a new package version *)
             add ~ctxt pkg_ver.ODBPkgVer.pkg dn)

          (fun e ->
             ODBFileUtil.rm ~ctxt ~recurse:true [dn]
             >>= fun () ->
             fail e)
    end

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

