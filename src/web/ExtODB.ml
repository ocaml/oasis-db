
(** This module implements service required by odb.ml an OASIS-DB experiment by
    thelema.
  
    More information here:
    https://github.com/thelema/odb

    @author Sylvain Le Gall
  *)

open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod
open XHTML.M
open Eliom_predefmod.Xhtml
open Lwt
open OASISVersion
open OASISTypes
open OCAAccount
open ODBGettext
open ODBPkgVer
open Template
open Common

module S = Sqlexpr

let () = 
  let create_table_odb db = 
    S.execute db
      sqlinit"CREATE TABLE IF NOT EXISTS odb\
        (pkg  TEXT NOT NULL,
         repo TEXT NOT NULL,
         ver  TEXT NOT NULL,
         CONSTRAINT one_pkg_per_repo UNIQUE(pkg, repo))"
  in
  let create_table_user db =
    S.execute db 
      sqlinit"CREATE TABLE IF NOT EXISTS odb_user\
        (user_id INTEGER NOT NULL)"
  in
  let create_table_program db =
    S.execute db
      sqlinit"CREATE TABLE IF NOT EXISTS odb_program\
        (program TEXT NOT NULL UNIQUE)"
  in
  let create_table_library db =
    S.execute db
      sqlinit"CREATE TABLE IF NOT EXISTS odb_library\
        (library TEXT NOT NULL UNIQUE)"
  in
  let rec upgrade db =
    function
      | 1 ->
          create_table_user db
          >>= fun () ->
          upgrade db 2
      | 2 ->
          S.execute db sql"DROP TABLE odb"
          >>= fun () ->
          create_table_odb db
          >>= fun () ->
          upgrade db 3
      | 3 ->
          create_table_program db
          >>= fun () ->
          upgrade db 4
      | 4 ->
          create_table_library db
          >>= fun () ->
          upgrade db 5
      | 5 ->
          S.execute db sql"DROP TABLE odb_library"
          >>= fun () ->
          create_table_library db
      | _ ->
          return ()
  in
  let init db =
    create_table_odb db 
    >>= fun () -> 
    create_table_user db
    >>= fun () ->
    create_table_program db
    >>= fun () ->
    create_table_library db
  in
    S.register "odb" 6 init upgrade

let template ~ctxt ~sp ttl ctnt =
 template
   ~ctxt
   ~sp
   ~title:(OneTitle ttl)
   ~div_id:"odb-experiment"
   ctnt

let index = 
  new_service 
    ["odb"]
    (suffix (all_suffix "path"))
    ()

let is_odb_admin ~ctxt () = 
  match Account.get_id ~ctxt () with 
    | Some id -> 
        begin
          if Context.is_admin ~ctxt () then
            return true
          else
            catch 
              (fun () ->
                 S.use ctxt.Context.sqle 
                   (fun db ->
                      S.select_one db 
                        sql"SELECT @d{user_id} FROM odb_user WHERE user_id = %d"
                        id
                      >>= fun _ ->
                      return ())
                 >>= fun () ->
                 return true)
              (function
                 | Not_found ->
                     return false
                 | e ->
                     fail e)
        end
    | None ->
        return false

(* Extract findlib root name from a findlib package name *)
let findlib_root fndlb_nm =
  let res, _ = 
    try 
      ExtLib.String.split fndlb_nm "."
    with ExtLib.Invalid_string ->
      fndlb_nm, ""
  in
    res

(* Compute installability of a build section. 
 * TODO: It should take into account default flag value 
 * + reasonable env like in oasis2debian
 *)
let is_bs_installable oasis bs = 
 match bs.bs_install with 
   | [OASISExpr.EBool true, true] -> true
   | _ -> false

(* Compute buildability of a build section. 
 * TODO: It should take into account default flag value 
 * + reasonable env like in oasis2debian
 *)
let is_bs_buildable oasis bs = 
 match bs.bs_build with 
   | [OASISExpr.EBool true, true] -> true
   | _ -> false

let get_odb_admin ~sp () =
  Context.get_user ~sp () 
  >>= fun (ctxt, accnt) ->
  is_odb_admin ~ctxt ()
  >>= 
    function
      | true -> return (ctxt, accnt)
      | false -> fail InsufficientAuth

let filename_of_ver ver = 
  ODBStorage.PkgVer.filename ver.pkg (string_of_version ver.ver)

module MapString = Map.Make(String)
module SetString = Set.Make(String)

(* Return a listing of tarball matching a list of package's version *)
let list_tarball ~ctxt mp = 
  let pkg_ver_lst =
    MapString.fold 
      (fun k v acc -> (k, v) :: acc)
      mp
      []
  in
    Lwt_list.fold_left_s
      (fun mp (pkg_str, ver_str) ->
         ODBStorage.PkgVer.filename pkg_str ver_str `Tarball
         >>= fun tarball_fn ->
         return 
           (MapString.add
              (FilePath.basename tarball_fn)
              (pkg_str, ver_str)
              mp))
      MapString.empty
      pkg_ver_lst

(* Return a listing of the repository *)
let list_repo ~ctxt nm = 
  S.use ctxt.Context.sqle
    (fun db ->
       S.fold db
         (fun mp (pkg, ver) ->
            pkg >>= fun pkg ->
            ver >>= fun ver ->
            return (MapString.add pkg ver mp))
         MapString.empty
         sql"SELECT @s{pkg}, @s{ver} FROM odb WHERE repo = %s" nm)

(* Return a listing of latest version *)
let list_latest ~ctxt () = 
  ODBStorage.Pkg.elements () 
  >>= fun pkg_lst ->
  Lwt_list.rev_map_p 
    (fun {ODBPkg.pkg_name = pkg_str} ->
       ODBStorage.PkgVer.latest pkg_str
       >|= fun pkg_ver ->
       pkg_str, string_of_version (pkg_ver.ODBPkgVer.ver))
    pkg_lst
  >>= fun lst ->
  Lwt_list.fold_left_s
    (fun mp (pkg_str, ver_str) -> 
       return (MapString.add pkg_str ver_str mp))
    MapString.empty 
    lst

module SetInt = Set.Make(struct type t = int let compare = ( - ) end)

(* Register an action that handle SQL query *)
let reg_sql_action ~name ~post_params f f' = 
  Action.register_new_post_coservice'
    ~name ~post_params
    (fun sp () p ->
       get_odb_admin ~sp () 
       >>= fun (ctxt, accnt) ->
       S.use ctxt.Context.sqle 
         (fun db -> f ctxt sp accnt p db)
       >>= fun res ->
       f' ctxt sp accnt p res)

(* Delete an administrator *)
let admin_delete = 
  reg_sql_action
    ~name:"odb_admin_delete"
    ~post_params:(int "id")
    (fun ctxt sp accnt id db ->
       S.execute db
         sql"DELETE FROM odb_user WHERE user_id = %d"
         id)
    (fun _ _ _ _ _ -> return ())

(* Add an administrator *)
let admin_add = 
  reg_sql_action
    ~name:"odb_admin_add"
    ~post_params:(int "id")
    (fun ctxt sp accnt id db ->
       S.execute db
         sql"INSERT INTO odb_user(user_id) VALUES (%d)"
         id)
    (fun _ _ _ _ _ -> return ())

(* Set a version for a repo/pkg *)
let repo_pkg_ver_set = 
  let fix_ver_opt f =
    function
      | Some ver -> 
          let ver_str = 
            string_of_version ver
          in
            if ver_str = "" then
              None
            else
              Some (f ver)
      | None -> 
          None
  in
    reg_sql_action
      ~name:"odb_repo_pkg_ver_set"
      ~post_params:(string "pkg" ** string "repo" ** opt (ExtParams.version "ver"))
      (fun ctxt sp accnt (pkg_str, (repo, ver_opt)) db ->
         S.execute db sql"BEGIN"
         >>= fun () ->
         catch 
           (fun () ->
              S.execute db 
                sql"DELETE FROM odb WHERE pkg = %s AND repo = %s"
                pkg_str repo
              >>= fun () ->
              begin
                match fix_ver_opt string_of_version ver_opt with
                  | Some ver ->
                      S.execute db
                        sql"INSERT INTO odb(pkg,repo,ver) \
                            VALUES (%s,%s,%s)"
                        pkg_str repo ver
                  | None ->
                      return ()
              end
              >>= fun () ->
              S.execute db sql"END")
           (fun e ->
              S.execute db sql"ROLLBACK"
              >>= fun () ->
              fail e))
      (fun ctxt sp accnt (pkg_str, (repo, ver_opt)) () ->
         Log.add ctxt.Context.sqle 
           (`Sys("odb", 
                 `VersionSet (pkg_str, 
                              repo, 
                              (fix_ver_opt (fun v -> v) ver_opt)))))

(* Clone a repository to another *)
let repo_pkg_clone =
  let snapshot_repo db nm =
    S.fold db 
      (fun acc (pkg, ver) ->
         pkg >>= fun pkg ->
         ver >>= fun ver ->
         return ((pkg, ver) :: acc))
      []
      sql"SELECT @s{pkg}, @s{ver} FROM odb WHERE repo = %s ORDER BY pkg DESC"
      nm
  in
    reg_sql_action
      ~name:"odb_repo_pkg_clone"
      ~post_params:(string "repo_src" ** string "repo_tgt")
      (fun ctxt sp accnt (repo_src, repo_tgt) db ->
         S.execute db sql"BEGIN"
         >>= fun () ->
         catch
           (fun () ->
              snapshot_repo db repo_src
              >>= fun lst_src ->

              snapshot_repo db repo_tgt
              >>= fun lst_tgt ->

              (* Update the DB *)
              S.execute db
                sql"DELETE FROM odb WHERE repo = %s"
                repo_tgt
              >>= fun () ->
              Lwt_list.iter_s
                (fun (pkg, ver) ->
                   S.execute db
                     sql"INSERT INTO odb(pkg,repo,ver) \
                         VALUES (%s,%s,%s)"
                     pkg repo_tgt ver)
                lst_src
              >>= fun () ->
              S.execute db sql"END"
              >>= fun () ->
              return (lst_src, lst_tgt)) 

           (fun e ->
              S.execute db sql"ROLLBACK"
              >>= fun () ->
              fail e))
      (fun ctxt _ _ (_, repo_tgt) (lst_src, lst_tgt) -> 
         (* Compute the version that has been changed *)
         let update_lst = 
           let rec comb acc =
             function
               | (((pkg_src, ver_src) :: tl_src) as lst_src), 
                 (((pkg_tgt, ver_tgt) :: tl_tgt) as lst_tgt)  ->
                   if pkg_src = pkg_tgt then
                     begin
                       if ver_src = ver_tgt then
                         comb 
                           acc 
                           (tl_src, tl_tgt)
                       else
                         comb 
                           ((pkg_src, Some ver_src) :: acc)
                           (tl_src, tl_tgt)
                     end

                   else if pkg_src < pkg_tgt then
                     begin
                       comb
                         ((pkg_src, Some ver_src) :: acc)
                         (tl_src, lst_tgt)
                     end

                   else
                     begin
                       comb 
                         ((pkg_tgt, None) :: acc)
                         (lst_src, tl_tgt)
                     end

               | [], lst_tgt ->
                   List.fold_left
                     (fun acc (pkg_tgt, _) ->
                        (pkg_tgt, None) :: acc)
                     acc
                     lst_tgt
               | lst_src, [] ->
                   List.fold_left
                     (fun acc (pkg_src, ver_src) ->
                        (pkg_src, Some ver_src) :: acc)
                     acc
                     lst_src
           in
             comb [] (lst_src, lst_tgt)
         in
           Lwt_list.iter_s
             (fun (pkg, ver_str_opt) ->
                let ver_opt =
                  match ver_str_opt with
                    | Some v -> Some (version_of_string v)
                    | None -> None
                in
                  Log.add ctxt.Context.sqle
                    (`Sys("odb", `VersionSet (pkg, repo_tgt, ver_opt))))
             update_lst)

(* Delete an external program *)
let program_delete = 
  reg_sql_action
    ~name:"odb_program_delete"
    ~post_params:(string "program")
    (fun ctxt sp accnt program db ->
       S.execute db
         sql"DELETE FROM odb_program WHERE program = %s"
         program)
    (fun _ _ _ _ _ -> return ())

(* Add an external program *)
let program_add = 
  reg_sql_action
    ~name:"odb_program_add"
    ~post_params:(string "program")
    (fun ctxt sp accnt program db ->
       S.execute db
         sql"INSERT INTO odb_program(program) VALUES (%s)"
         program)
    (fun _ _ _ _ _ -> return ())

(* List external programs *)
let program_list ~ctxt () =
  S.use ctxt.Context.sqle
    (fun db ->
       S.fold db
         (fun acc program -> 
            program >>= fun program ->
            return (program :: acc))
         []
         sql"SELECT @s{program} FROM odb_program ORDER BY program DESC")

(* Delete an external library *)
let library_delete = 
  reg_sql_action
    ~name:"odb_library_delete"
    ~post_params:(string "library")
    (fun ctxt sp accnt library db ->
       S.execute db
         sql"DELETE FROM odb_library WHERE library = %s"
         library)
    (fun _ _ _ _ _ -> return ())

(* Add an external library *)
let library_add = 
  reg_sql_action
    ~name:"odb_library_add"
    ~post_params:(string "library")
    (fun ctxt sp accnt library db ->
       S.execute db
         sql"INSERT INTO odb_library(library) VALUES (%s)"
         library)
    (fun _ _ _ _ _ -> return ())

(* List external libraries *)
let library_list ~ctxt () =
  S.use ctxt.Context.sqle
    (fun db ->
       S.fold db
         (fun acc library -> 
            library >>= fun library ->
            return (library :: acc))
         []
         sql"SELECT @s{library} FROM odb_library ORDER BY library DESC")

(* Data returned by map_info_name when oasis is present *)
type info_t = 
    {
      is_program: bool;
      is_library: bool;
      oasis:      OASISTypes.package;
    }

(* Create a map that link info's name with pkg_str/ver_str/oasis
 * Info's name is either: findlib root library, executable name,
 * package name if no _oasis exists or external program defined
 * by the administrator 
 *)
let map_info_name ~ctxt mp_repo = 
  let add_merge_entry nm e (msg, mp) =
    try 
      let frmr =
        MapString.find nm mp
      in
      let check_pkg_ver (pkg_str1, ver_str1) (pkg_str2, ver_str2) msg = 
        if pkg_str1 <> pkg_str2 ||
           ver_str1 <> ver_str2 then
          (Printf.sprintf
             (f_ "Info %s is provided at the same time by \
                  package %s v%s and %s v%s")
             nm
             pkg_str1 ver_str1
             pkg_str2 ver_str2)
          :: msg
        else 
          msg
      in
      let err_pkg_ver_prog (pkg_str, ver_str) prog msg =
        (Printf.sprintf 
           "Info %s is provided at the same time by package \
            %s v%s and external program %s"
           nm 
           pkg_str ver_str prog)
        :: msg
      in
      let err_pkg_ver_lib (pkg_str, ver_str) lib msg =
        (Printf.sprintf
           "Info %s is provided at the same time by package \
            %s v%s and external library %s"
           nm 
           pkg_str ver_str lib)
        :: msg
      in
      let err_prog_lib prog lib msg =
        (Printf.sprintf
           "Info %s is provided at the same time by external program %s \
            and external library %s"
           nm prog lib)
        :: msg
      in

      let msg, mp =
        match frmr, e with 
          | `PkgVerOASIS (pkg_ver1, t1), `PkgVerOASIS (pkg_ver2, t2) ->
              let msg = 
                check_pkg_ver pkg_ver1 pkg_ver2 msg
              in
              let e = 
                `PkgVerOASIS 
                  (pkg_ver1,
                   {t1 with is_program = t1.is_program || t2.is_program;
                            is_library = t1.is_library || t2.is_library})
              in
                msg, MapString.add nm e mp

          | `PkgVerOASIS (pkg_ver1, t1), `PkgVer pkg_ver2 ->
              check_pkg_ver pkg_ver1 pkg_ver2 msg, mp

          | `PkgVerOASIS (pkg_ver1, _), `Program prg2 ->
              err_pkg_ver_prog pkg_ver1 prg2 msg, mp

          | `PkgVerOASIS (pkg_ver1, _), `Library lib2 ->
              err_pkg_ver_lib pkg_ver1 lib2 msg, mp

          | `PkgVer pkg_ver1, `PkgVerOASIS (pkg_ver2, t2) ->
              check_pkg_ver pkg_ver1 pkg_ver2 msg,
              MapString.add nm e mp

          | `PkgVer pkg_ver1, `PkgVer pkg_ver2 ->
              check_pkg_ver pkg_ver1 pkg_ver2 msg, mp

          | `PkgVer pkg_ver1, `Program prg2 ->
              err_pkg_ver_prog pkg_ver1 prg2 msg, mp

          | `PkgVer pkg_ver1, `Library lib2 ->
              err_pkg_ver_lib pkg_ver1 lib2 msg, mp

          | `Program prg1, `PkgVerOASIS (pkg_ver2, t2) ->
              err_pkg_ver_prog pkg_ver2 prg1 msg,
              MapString.add nm e mp
              
          | `Program prg1, `PkgVer pkg_ver2 ->
              err_pkg_ver_prog pkg_ver2 prg1 msg, 
              MapString.add nm e mp
              
          | `Program prg1, `Program prg2 ->
              msg, mp

          | `Program prg1, `Library lib2 ->
              err_prog_lib prg1 lib2 msg, mp

          | `Library lib1, `PkgVerOASIS (pkg_ver2, t2) ->
              err_pkg_ver_prog pkg_ver2 lib1 msg,
              MapString.add nm e mp
              
          | `Library lib1, `PkgVer pkg_ver2 ->
              err_pkg_ver_lib pkg_ver2 lib1 msg, 
              MapString.add nm e mp
              
          | `Library lib1, `Program prog2 ->
              err_prog_lib prog2 lib1 msg, mp

          | `Library lib1, `Library prg2 ->
              msg, mp
      in
        msg, mp

    with Not_found ->
      msg, MapString.add nm e mp
  in


  let lst_repo = 
    MapString.fold 
      (fun pkg_str ver_str acc -> (pkg_str, ver_str) :: acc)
      mp_repo
      []
  in
  let add_with_oasis pkg_str ver_str oasis acc =
    let t = 
      {
        is_library = false;
        is_program = false;
        oasis      = oasis;
      }
    in
    let pkg_ver_str =
      (pkg_str, ver_str)
    in
    let fndlb_nm_mp = 
      OASISLibrary.findlib_name_map oasis
    in
    let acc = 
      List.fold_left 
        (fun acc ->
           function
             | Executable (cs, bs, _) ->
                 (* Add executable provides *)
                 if is_bs_installable oasis bs then
                   add_merge_entry
                     cs.cs_name 
                     (`PkgVerOASIS(pkg_ver_str, {t with is_program = true}))
                     acc
                 else
                   acc

             | Library (cs, bs, _) ->
                 (* Add findlib provides *)
                 if is_bs_installable oasis bs then
                   let fndlb_nm =
                     OASISLibrary.findlib_of_name 
                       ~recurse:true 
                       fndlb_nm_mp 
                       cs.cs_name 
                   in
                   let fndlb_root =
                     findlib_root fndlb_nm
                   in
                     add_merge_entry
                       fndlb_root
                       (`PkgVerOASIS(pkg_ver_str, {t with is_library = true}))
                       acc
                 else
                   acc

             | SrcRepo _ | Test _ | Doc _ | Flag _ ->
                 acc)
        acc
        oasis.sections 
    in
      acc
  in

  let add_without_oasis pkg_str ver_str acc =
    add_merge_entry pkg_str (`PkgVer (pkg_str, ver_str)) acc
  in

  let add_ext_program program acc =
    add_merge_entry program (`Program program) acc
  in

  let add_ext_library library acc =
    add_merge_entry library (`Library library) acc
  in

    Lwt_list.rev_map_p 
      (fun (pkg_str, ver_str) ->
         ODBStorage.PkgVer.filename pkg_str ver_str `OASIS
         >>= fun oasis_fn ->
         catch 
           (fun () ->
              ODBOASIS.from_file ~ctxt:ctxt.Context.odb oasis_fn
              >>= fun oasis ->
              return (pkg_str, ver_str, Some oasis))
           (fun _ ->
              return (pkg_str, ver_str, None)))
      lst_repo
    >>= fun lst_repo_pkgver ->
    program_list ~ctxt ()
    >>= fun lst_ext_program ->
    library_list ~ctxt ()
    >>= fun lst_ext_library ->
    begin
      let acc =
        [], MapString.empty
      in
      let acc =
        List.fold_left
          (fun acc (pkg_str, ver_str, oasis_opt) ->
             match oasis_opt with 
               | Some oasis -> 
                   add_with_oasis pkg_str ver_str oasis acc

               | None -> 
                   add_without_oasis pkg_str ver_str acc)
          acc
          lst_repo_pkgver
      in
      let acc =
        List.fold_left
          (fun acc program ->
             add_ext_program program acc)
          acc
          lst_ext_program
      in
      let acc =
        List.fold_left
          (fun acc library ->
             add_ext_library library acc)
          acc
          lst_ext_library
      in
        return acc
    end

(* Compute non optional dependency out of t datastructure *)
let non_optional_deps ~ctxt oasis = 
  ODBDeps.solve ~ctxt:ctxt.Context.odb 
    (ODBDeps.of_oasis_package oasis)
  >>= fun deps ->
  let non_optional_deps = 
    (* TODO: move this to ODBDeps *)
    (* Compute tool dependencies *)
    List.fold_left 
      (fun acc ->
         function 
           | Library (_, bs, _) 
           | Executable (_, bs, _) ->
               if is_bs_buildable oasis bs then
                 List.fold_left
                   (fun acc ->
                      function
                        | ExternalTool nm ->
                            (MapString.add nm None acc)
                        | InternalExecutable _ ->
                            acc)
                   acc
                   bs.bs_build_tools
               else
                 acc
           | Test _ | Doc _ | Flag _ | SrcRepo _ ->
               acc)
      MapString.empty 
      oasis.sections
  in
  let non_optional_deps =
    ODBDeps.fold 
      (fun fndlb_nm e acc ->
         let fndlb_root = 
           findlib_root fndlb_nm 
         in
           if not e.ODBDeps.optional then
             (* TODO: merge with previous version *)
             MapString.add fndlb_root e.ODBDeps.version_cmp acc
           else
             acc)
      deps
      non_optional_deps
  in
    return non_optional_deps

type table_t =
    {
      pkg_str:               string;
      ver_str_stable_opt:    string option;
      ver_str_testing_opt:   string option;
      ver_str_unstable:      string;
    }

let table_packages_tr ~sp pkg_str lst =
  tr
    (th [a Common.view sp [pcdata pkg_str] (pkg_str, NoVersion)])
    lst

let table_packages_td ~sp table_t repo ver_str_opt extra =
  let ver_class = 
    match ver_str_opt with
      | Some ver_str' ->
          if ver_str' = table_t.ver_str_unstable then
            "odb-latest"
          else
            "odb-not-latest"
      | None ->
          "odb-none"
  in
  let ctnt =
    match ver_str_opt with 
      | Some ver_str ->
          [a Common.view sp [pcdata ver_str] 
             (table_t.pkg_str, Version (version_of_string ver_str))]
      | None ->
          [pcdata (s_ "none")]
  in
    td ~a:[a_class ["odb-"^repo; ver_class]] (ctnt @ extra)

(* Gather table data to build table_packages_box and table_packages_edit_box
 *)
let table_packages_data ~ctxt ~sp () = 
  list_repo ~ctxt "stable"
  >>= fun mp_stable ->
  list_repo ~ctxt "testing"
  >>= fun mp_testing ->
  list_latest ~ctxt ()
  >>= fun mp_latest ->
  begin
    
    let find_exnless mp k =
      try 
        Some (MapString.find k mp)
      with Not_found ->
        None
    in

    let mk_pkg pkg_str ver_str = 
      {
        pkg_str             = pkg_str;
        ver_str_stable_opt  = find_exnless mp_stable pkg_str;
        ver_str_testing_opt = find_exnless mp_testing pkg_str;
        ver_str_unstable    = ver_str;
      }
    in

    let lst =
      (MapString.fold 
         (fun k v acc -> (mk_pkg k v) :: acc)
         mp_latest
         [])
    in
      return (mp_stable, mp_testing, mp_latest, List.rev lst)
  end

let table_packages_box_header_hd, table_packages_box_header_tl  =
 (th ~a:[a_class ["col-pkg"]]  [pcdata (s_ "Package")]),
 [th ~a:[a_class ["col-repo"]] [pcdata (s_ "Stable")];
  th ~a:[a_class ["col-repo"]] [pcdata (s_ "Testing")];
  th ~a:[a_class ["col-repo"]] [pcdata (s_ "Unstable")]]

(* Build a table that shows state of package/version for 3 repositories
 * ("stable", "testing", "unstable")
 *)
let table_packages_box ~ctxt ~sp () = 
  table_packages_data ~ctxt ~sp ()
  >>= fun (_, _, _, lst) ->
  begin
    let mk_table hd tl =
      odd_even_table 
        (tr
           table_packages_box_header_hd
           table_packages_box_header_tl)
        (hd :: tl)
    in

    let res = 
      ncol_table
        ~a:[a_id "odb-list"]
        ~mk_table
        2
        (tr (td ~a:[a_colspan 4] [pcdata "\030"]) []) 
        (List.map
           (fun table_t ->
              let mk_td repo ver_str_opt = 
                table_packages_td ~sp table_t repo ver_str_opt []
              in
                table_packages_tr ~sp 
                  table_t.pkg_str
                  [mk_td "stable" table_t.ver_str_stable_opt;
                   mk_td "testing" table_t.ver_str_testing_opt;
                   mk_td "unstable" (Some table_t.ver_str_unstable)])
           lst)
    in
      return res
  end

(* Compute dependency errors for package *)
let packager_dependency_error ~ctxt mp_info_name pkg_str ver_str =
  ODBStorage.PkgVer.filename pkg_str ver_str `OASIS
  >>= fun oasis_fn ->
  catch 
    (fun () ->
       ODBOASIS.from_file ~ctxt:ctxt.Context.odb oasis_fn
       >>= fun oasis ->
       non_optional_deps ~ctxt oasis
       >|= fun non_optional_deps ->
       MapString.fold
         (fun dep_str ver_cmp_opt errors ->
            try
              match MapString.find dep_str mp_info_name with 
                | `PkgVerOASIS ((pkg_str', ver_str'), _) 
                | `PkgVer (pkg_str', ver_str') ->
                    begin
                      match ver_cmp_opt with 
                        | Some cmp ->
                            let ver_ok = 
                              comparator_apply 
                                (version_of_string ver_str')
                                cmp
                            in
                              if not ver_ok then
                                (Printf.sprintf 
                                   (f_ "Package %s v%s need %s (%s) \
                                        but only v%s is provided")
                                   pkg_str ver_str dep_str
                                   (string_of_comparator cmp)
                                   ver_str')
                                :: errors
                              else
                                errors

                        | None ->
                            errors
                    end
                | `Program _ ->
                    begin
                      match ver_cmp_opt with 
                        | Some cmp ->
                            (Printf.sprintf 
                               (f_ "Package %s v%s need %s (%s) \
                                    but only a program is provided")
                               pkg_str ver_str dep_str
                               (string_of_comparator cmp))
                            :: errors

                        | None ->
                            errors
                    end

                | `Library _ ->
                    begin
                      match ver_cmp_opt with 
                        | Some cmp ->
                            (Printf.sprintf 
                               (f_ "Package %s v%s need %s (%s) \
                                    but only a library is provided")
                               pkg_str ver_str dep_str
                               (string_of_comparator cmp))
                            :: errors

                        | None ->
                            errors
                    end

            with Not_found ->
              begin
                match ver_cmp_opt with 
                  | Some cmp ->
                      (Printf.sprintf 
                         (f_ "Package %s v%s need %s (%s)")
                         pkg_str ver_str dep_str
                         (string_of_comparator cmp))
                      :: errors
                  | None ->
                      (Printf.sprintf
                         (f_ "Package %s v%s need %s")
                         pkg_str ver_str dep_str)
                      :: errors
              end)
         non_optional_deps
         [])
    (fun _ ->
       (* No _oasis to reason about *)
       return [])

(* Build a table that allow to edit the state of "testing" repository
 *)
let table_packages_edit_box ~ctxt ~sp repo = 
  let mk_error mp_info_name pkg_str ver_str_opt errors = 
    match ver_str_opt with
      | Some ver_str ->
          packager_dependency_error ~ctxt mp_info_name pkg_str ver_str
          >|= 
          begin
            function
              | [] ->
                  (pcdata ""), errors
              | lst ->
                  let id =
                    Printf.sprintf "error%d" (List.length errors)
                  in
                    XHTML.M.a
                      ~a:[a_href (uri_of_string ("#"^id));
                          a_class ["odb-dep-error"]]
                      [img 
                         ~alt:(s_ " Error") 
                         ~src:(make_uri ~service:(static_dir sp) ~sp ["dep-error.png"])
                         ~a:[a_title (String.concat (s_ "; ") lst)]
                         ()],
                    (id, lst) :: errors
          end
      | None ->
          return ((pcdata ""), errors)
  in
  table_packages_data ~ctxt ~sp () 
  >>= fun (mp_stable, mp_testing, mp_unstable, lst) ->
  map_info_name ~ctxt mp_stable
  >>= fun (msg_stable, mp_info_name_stable) ->
  map_info_name ~ctxt mp_testing
  >>= fun (msg_testing, mp_info_name_testing) ->
  map_info_name ~ctxt mp_unstable
  >>= fun (msg_unstable, mp_info_name_unstable) ->
  
  Lwt_list.rev_map_p
    (fun table_t ->
       (* Build the select for testing change *)
       ODBStorage.PkgVer.elements table_t.pkg_str 
       >>= fun pkg_ver_lst ->

       (* Compute errors *)
       mk_error mp_info_name_stable table_t.pkg_str table_t.ver_str_stable_opt []
       >>= fun (icon_stable_error, errors) ->
       mk_error mp_info_name_testing table_t.pkg_str table_t.ver_str_testing_opt errors
       >>= fun (icon_testing_error, errors) ->
       mk_error mp_info_name_unstable table_t.pkg_str (Some table_t.ver_str_unstable) errors
       >>= fun (icon_unstable_error, errors) ->

       begin
         let lst = 
           List.map 
             (fun pkg_ver -> 
                let ver = pkg_ver.ODBPkgVer.ver in
                let ver_str = string_of_version ver in
                  Option([],
                         ver,
                         Some (pcdata ver_str),
                         Some ver_str = table_t.ver_str_testing_opt))
             pkg_ver_lst
         in

         let action_set =
           post_form
             ~service:repo_pkg_ver_set
             ~sp 
             (fun (pkg_nm, (repo_nm, ver_nm)) ->
                let ctnt =
                  match lst with 
                    | hd :: tl ->
                        [string_input ~name:pkg_nm ~input_type:`Hidden ~value:table_t.pkg_str ();
                         string_input ~name:repo_nm ~input_type:`Hidden ~value:repo ();
                         user_type_select 
                           string_of_version
                           ~name:ver_nm
                           (Option([], 
                                   (version_of_string ""), 
                                   Some (pcdata (s_ "None")), 
                                   None = table_t.ver_str_testing_opt))
                           (hd :: tl);
                         string_input ~input_type:`Submit ~value:(s_ "Set") ()]
                    | [] ->
                        []
                in
                  [div ctnt])
             ()
         in

         let tr =
            let mk_td repo ver_str_opt icon = 
              table_packages_td ~sp table_t repo ver_str_opt [icon]
            in
              table_packages_tr ~sp 
                table_t.pkg_str
                [mk_td "stable" table_t.ver_str_stable_opt icon_stable_error;
                 mk_td "testing" table_t.ver_str_testing_opt icon_testing_error;
                 mk_td "unstable" (Some table_t.ver_str_unstable) icon_unstable_error;
                 td [div [action_set]];
                ]
         in
           return (errors, tr)
       end)
    lst
  >|= fun tr_error_lst ->
  begin
    let (error_lst, tr_lst) = 
      List.split tr_error_lst
    in
      odd_even_table 
        (tr
           table_packages_box_header_hd
           (table_packages_box_header_tl @ 
            [th ~a:[a_class ["col-action"]] [pcdata (s_ "Action")]]))
        (List.rev tr_lst),
      msg_stable,
      msg_testing,
      msg_unstable,
      (List.flatten error_lst)
  end

let xxx_management_box ~ctxt ~sp xxx_title xxx xxx_col xxx_add_button add delete xxx_lst = 
  let tab =
    let mk_tr e = 
      tr 
        (td [pcdata e])
        [td 
           [post_form 
              ~service:delete
              ~sp
              (fun name ->
                 [div [string_button ~name ~value:e
                         [pcdata (s_ "Remove")]]])
              ()]]
    in
      ncol_table
        ~a:[a_id ("odb-admin-"^xxx^"-list")]
        ~mk_table:(fun hd tl ->
                     odd_even_table
                       (tr
                          (th ~a:[a_class ["col-"^xxx]]   [pcdata (s_ xxx_col)])
                          [th ~a:[a_class ["col-action"]] [pcdata (s_ "Action")]])
                       (hd :: tl))
        3
        (tr (td ~a:[a_colspan 2] [pcdata "\030"]) [])
        (List.map mk_tr xxx_lst)
  in

  let add_box =
    post_form
      ~service:add
      ~sp
      (fun name ->
         [p
            [string_input ~name ~input_type:`Text ();
             string_input 
               ~input_type:`Submit 
               ~value:xxx_add_button
               ()]])
      ()
  in

    return 
      (div 
         ~a:[a_id ("odb-admin-"^xxx)]
         [h3 [pcdata xxx_title];
          tab;
          add_box])

let program_management_box ~ctxt ~sp () = 
  program_list ~ctxt ()
  >>= 
  xxx_management_box ~ctxt ~sp 
    (s_ "External program management")
    "program"
    (s_ "Program")
    (s_ "Add external program")
    program_add
    program_delete

let library_management_box ~ctxt ~sp () = 
  library_list ~ctxt ()
  >>= 
  xxx_management_box ~ctxt ~sp 
    (s_ "External library management")
    "library"
    (s_ "Library")
    (s_ "Add external library")
    library_add
    library_delete

let user_management_box ~ctxt ~sp () = 
  Account.list ~ctxt () 
  >>= fun accnt_lst ->
  S.use ctxt.Context.sqle
    (fun db ->
       S.fold db
         (fun st id -> id >>= fun id -> return (SetInt.add id st))
         SetInt.empty
         sql"SELECT @d{user_id} FROM odb_user")
  >>= fun admin_st ->
  let lst_admin, lst_other = 
    List.fold_left
      (fun (adm, oth) accnt ->
         if Context.is_admin ~ctxt ~accnt:(Some accnt) () ||
            SetInt.mem accnt.accnt_id admin_st then
           (accnt :: adm) , oth
         else
           adm, (accnt :: oth))
      ([], [])
      accnt_lst
  in
  let () = 
    List.iter (fun accnt -> prerr_endline accnt.accnt_real_name) accnt_lst
  in
  let tab = 
    let mk_tr accnt = 
      let real_name = 
        accnt.accnt_real_name
      in
      if Context.is_admin ~ctxt ~accnt:(Some accnt) () then
        (* OASIS-DB admin, cannot remove *)
        tr
          (td 
             ~a:[a_colspan 2]
             [pcdata (Printf.sprintf (f_ "%s (oasis-db admin)") real_name)])
          []
      else
        tr
          (td [pcdata real_name])
          [td
             [post_form 
                ~service:admin_delete
                ~sp
                (fun id_nm ->
                   [div [int_button ~name:id_nm ~value:accnt.accnt_id
                           [pcdata (s_ "Remove")]]])
                ()]]
    in
      ncol_table 
        ~a:[a_id "odb-admin-user-list"]
        ~mk_table:(fun hd tl ->
                     odd_even_table
                       (tr
                          (th ~a:[a_class ["col-user"]]   [pcdata (s_ "User")])
                          [th ~a:[a_class ["col-action"]] [pcdata (s_ "Action")]])
                       (hd :: tl))
        3
        (tr (td ~a:[a_colspan 2] [pcdata "\030"]) [])
        (List.map mk_tr lst_admin)
  in

  let lst_other =
    List.sort
      (fun e1 e2 ->
         String.compare e1.accnt_real_name e2.accnt_real_name)
      lst_other
  in

  let select_add = 
    match lst_other with 
      | hd :: tl ->
          let mk_option ?(selected=false) accnt =
            Option([], 
                   accnt.accnt_id, 
                   Some (pcdata accnt.accnt_real_name), 
                   selected)
          in
            post_form
              ~service:admin_add
              ~sp
              (fun id_nm ->
                 [p
                    [int_select 
                       ~name:id_nm
                       (mk_option ~selected:true hd)
                       (List.map mk_option tl);
                     string_input 
                       ~input_type:`Submit 
                       ~value:(s_ "Add admin.") 
                       ()]])
              ()

      | [] ->
          pcdata "No user to add"
  in

    return 
      (div 
         ~a:[a_id "odb-admin-user"]
         [h3 [pcdata (s_ "User management")];
          tab;
          select_add])


let package_management_box ~ctxt ~sp () = 
  table_packages_edit_box ~ctxt ~sp "testing"
  >>= fun (table_packages_edit_box, msg_stable, msg_testing, msg_unstable, msg_pkg)  ->
  return 
    (div 
       ~a:[a_id "odb-admin-package"]
       [h3 [pcdata (s_ "Package management")];
        table_packages_edit_box;
        div
          [post_form 
             ~service:repo_pkg_clone
             ~sp
             (fun (repo_src_nm, repo_tgt_nm) ->
                [p [string_input ~name:repo_src_nm ~input_type:`Hidden ~value:"testing" ();
                    string_input ~name:repo_tgt_nm ~input_type:`Hidden ~value:"stable" ();
                    string_input ~input_type:`Submit ~value:(s_ "Promote testing to stable") ()]])
             ()];
       ],
     if msg_stable <> [] || msg_testing <> [] || msg_unstable <> [] || msg_pkg <> [] then
       begin
         let mk_repo_error nm msg =
            match msg with 
              | hd :: tl ->
                  [
                    h4 [pcdata nm];  
                    ul 
                      (li [pcdata hd])
                      (List.map (fun e -> li [pcdata e]) tl)
                  ]
              | [] ->
                  []
         in
         let mk_pkg_error nm msg =
           let mk_error acc (id, lst) = 
             match lst with 
               | hd :: tl ->
                   (li ~a:[a_id id] [pcdata hd])
                   ::
                   (List.fold_left
                      (fun acc e -> (li [pcdata e]) :: acc)
                      acc
                      tl)
               | [] ->
                   acc
           in

           match List.fold_left mk_error [] msg with
             | hd :: tl ->
                 [h4 [pcdata nm]; ul hd tl]
             | [] ->
                 []
         in
         div
           ~a:[a_id "odb-admin-error"]
           (h3 [pcdata (s_ "Errors")]
            ::
            (mk_repo_error (s_ "Stable") msg_stable
             @
             mk_repo_error (s_ "Testing") msg_testing
             @
             mk_repo_error (s_ "Unstable") msg_unstable
             @
             mk_pkg_error (s_ "Packages") msg_pkg))
       end
     else
       begin
         pcdata ""
       end
    )

let admin = 
  register_new_service
    ~path:["admin"; "odb"]
    ~get_params:unit
     (fun sp () () ->
        Context.get ~sp () 
        >>= fun ctxt ->
        is_odb_admin ~ctxt ()
        >>= 
          function
            | true ->
                user_management_box ~ctxt ~sp () 
                >>= fun user_management_box ->
                package_management_box ~ctxt ~sp ()
                >>= fun (package_management_box, error_box) ->
                program_management_box ~ctxt ~sp ()
                >>= fun program_management_box ->
                library_management_box ~ctxt ~sp ()
                >>= fun library_management_box ->
                template ~ctxt ~sp
                  "ODB admin panel"
                  [user_management_box;
                   package_management_box;
                   program_management_box;
                   library_management_box;
                   error_box;
                   div 
                     ~a:[a_class ["action"]]
                     [ul
                       (li [a index sp [pcdata (s_ "Public ODB page")] []])
                       []]]
            | false ->
                fail InsufficientAuth) 

let mk_info ~ctxt pkg_str ver_str t_opt = 
  let mk_info_t tarball_bn t = 
    non_optional_deps ~ctxt t.oasis
    >>= fun non_optional_deps ->

    let non_optional_deps_lst = 
      MapString.fold
        (fun nm vcmp_opt acc ->
           let dep = 
             match vcmp_opt with 
               | Some vcmp ->
                   Printf.sprintf "%s(%s)" 
                     nm
                     (* Get rid of white space *)
                     (ExtLib.String.replace_chars
                        (function
                           | ' ' -> ""
                           | c -> ExtLib.String.of_char c)
                        (string_of_comparator vcmp))
               | None ->
                   nm
           in
             dep :: acc)
        non_optional_deps
        []
    in
    let content = 
      Printf.sprintf 
        "deps=%s\n\
         tarball=%s\n\
         is_library=%b\n\
         is_program=%b"
        (String.concat "," non_optional_deps_lst)
        tarball_bn
        t.is_library t.is_program
    in
      return content
  in
    ODBStorage.PkgVer.filename pkg_str ver_str `Tarball
    >>= fun tarball_fn ->
    begin
      let tarball_bn = FilePath.basename tarball_fn in
        match t_opt with 
          | Some t ->
              mk_info_t tarball_bn t
          | None -> 
              return 
                (Printf.sprintf
                   "# no valid _oasis\n\
                    tarball=%s"
                   tarball_bn)
    end

let () = 
  Any.register
    index
    (fun sp path () ->
       Context.get ~sp () 
       >>= fun ctxt ->

       begin
         let mk_tr fn = 
           let path'  = 
             if fn = ".." then
               List.rev (List.tl (List.rev  path))
             else
               path @ [fn]
           in
             tr 
               (td 
                  ~a:[a_class ["n"]] 
                  [Xhtml.a index sp [pcdata fn] path']) 
               []
         in

         let dir_list nm ?hd lst = 
           let tl = 
             List.rev_map mk_tr lst
           in
             template ~ctxt ~sp 
               (Printf.sprintf "Exploring repository %s" nm)
               [table (mk_tr "..") 
                  (match hd with 
                     | Some lst -> 
                         List.rev_append (List.rev_map mk_tr lst) tl
                     | None -> 
                         tl)]
             >>= 
             Xhtml.send ~sp
         in

         let path = 
           List.filter (( <> ) "") path
         in

         let repo nm path =
           begin
             match nm with 
               | "unstable" ->
                   list_latest ~ctxt ()
               | nm ->
                   list_repo ~ctxt nm
           end
           >>= fun mp_repo ->
           begin
             match path with 
               | ["pkg"] | ["pkg"; "backup"] ->
                   begin
                     list_tarball ~ctxt mp_repo
                     >>= fun mp_tarball ->
                     begin
                       let lst = 
                         MapString.fold
                           (fun tarball_bn _ acc -> tarball_bn :: acc)
                           mp_tarball
                           []
                       in
                       let hd = 
                         if List.mem "backup" path then
                           []
                         else
                           ["info"; "backup"]
                       in
                         dir_list nm ~hd  lst
                     end
                   end

               | ["pkg"; "info"] ->
                   begin
                     map_info_name ~ctxt mp_repo
                     >>= fun (_, mp_info_name) ->
                     dir_list nm
                       (MapString.fold 
                          (fun nm _ acc -> nm :: acc) 
                          mp_info_name 
                          [])
                   end

               | [] ->
                   dir_list nm 
                     ["pkg"]

               | ["pkg"; tarball_bn] ->
                   begin
                     list_tarball ~ctxt mp_repo 
                     >>= fun mp_tarball ->
                     try 
                       let (pkg_str, ver_str) = 
                         MapString.find tarball_bn mp_tarball
                       in
                         ODBStorage.PkgVer.find pkg_str ver_str 
                         >>= fun pkg_ver ->
                         begin
                           match pkg_ver.publink with 
                             | Some uri_str ->
                                 String_redirection.send ~sp 
                                   (uri_of_string uri_str)
                             | None ->
                                 ODBStorage.PkgVer.filename pkg_str ver_str `Tarball
                                 >>= fun fn -> 
                                 Files.send ~sp 
                                   (FilePath.make_absolute 
                                      (FileUtil.pwd ())
                                      fn)
                         end

                     with Not_found ->
                       fail Eliom_common.Eliom_404
                   end

               | ["pkg"; "backup"; tarball_bn] ->
                   begin
                     list_tarball ~ctxt mp_repo 
                     >>= fun mp_tarball ->
                     try 
                       let (pkg_str, ver_str) = 
                         MapString.find tarball_bn mp_tarball
                       in
                         ODBStorage.PkgVer.filename pkg_str ver_str `Tarball
                         >>= fun fn -> 
                         Files.send ~sp 
                           (FilePath.make_absolute 
                              (FileUtil.pwd ())
                              fn)

                     with Not_found ->
                       fail Eliom_common.Eliom_404
                   end

               | ["pkg"; "info"; info_str] ->
                   begin
                       map_info_name ~ctxt mp_repo
                       >>= fun (_, mp_info_name) ->
                       begin
                         try
                           let t = 
                             MapString.find info_str mp_info_name 
                           in
                           let ctnt =
                             match t with 
                               | `PkgVerOASIS ((pkg_str, ver_str), t) ->
                                   mk_info ~ctxt pkg_str ver_str (Some t)
                               | `PkgVer (pkg_str, ver_str) ->
                                   mk_info ~ctxt pkg_str ver_str None
                               | `Program _ ->
                                   return
                                     ("# External program added by administrator\n\
                                       is_program=true")
                               | `Library _ ->
                                   return
                                     ("# External library added by administrator\n\
                                       is_library=true")
                           in
                             ctnt
                             >>= fun content ->
                             Text.send ~sp (content, "text/plain")

                         with Not_found ->
                           fail Eliom_common.Eliom_404
                       end
                   end

               | _ ->
                   fail Eliom_common.Eliom_404
           end
         in

           match path with 
             | [] ->
                 Mkd.load ctxt.Context.mkd "ext-odb"              
                 >>= fun intro ->
                 table_packages_box ~ctxt ~sp ()
                 >>= fun tbl_pkg_box ->
                 is_odb_admin ~ctxt ()
                 >>= fun is_odb_admin ->

                 template ~ctxt ~sp 
                   (s_ "ODB, a simple package installer")
                   (intro @ 
                    [tbl_pkg_box;
                     div 
                       ~a:[a_class ["action"]]
                       [
                         if is_odb_admin then
                           ul
                             (li [a admin sp [pcdata (s_ "Admin panel")] ()])
                             []
                         else
                           pcdata ""
                       ]

                    ])
                 >>= 
                 Xhtml.send ~sp

             | ("stable" as nm) :: tl ->
                 repo nm tl 

             | ("testing" as nm) :: tl ->
                 repo nm tl 

             | ("unstable" as nm) :: tl ->
                 repo nm tl

             | _ ->
                 fail Eliom_common.Eliom_404

       end)
