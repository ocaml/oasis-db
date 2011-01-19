
open OASISVersion
open OASISTypes
open ODBPkgVer
open OASISUtils
open Lwt

type elem =
    {
      optional:        bool;
      version_cmp:     comparator option;
      package_version: ODBPkgVer.t option;
    }

type t = elem MapString.t

let add findlib_nm e1 t = 
  let e2 =
    try 
      MapString.find findlib_nm t
    with Not_found ->
      {
        optional        = true;
        version_cmp     = None;
        package_version = None;
      }
  in
  let version_cmp = 
    match e1.version_cmp, e2.version_cmp with
      | Some cmp1, Some cmp2 ->
          Some (comparator_reduce (VAnd (cmp1, cmp2)))
      | None, ver_opt
      | ver_opt, None ->
          ver_opt
  in
  let optional =
    e1.optional && e2.optional
  in
  let package_version = 
    match e1.package_version, e2.package_version with 
      | (Some _ as e), _
      | _, (Some _ as e) ->
          e
      | None, None ->
          None
  in
    MapString.add 
      findlib_nm 
      {
        optional = optional;
        version_cmp = version_cmp;
        package_version = package_version;
      }
      t

(* TODO: tool dependencies which includes test/doc, for dependencies
 * and provides.
 *)
(* Extract build dependencies from OASIS package *)
let of_oasis_package pkg = 
  List.fold_left
    (fun acc ->
       function
         | Executable (_, bs, _)
         | Library (_, bs, _) ->
             begin
               let optional =
                 (* If the build is optional, the dependencies
                  * are optional as well 
                  *)
                 match bs.bs_build with 
                   | [OASISExpr.EBool true, true] -> false
                   | _ -> true
               in
                 List.fold_left
                   (fun acc ->
                      function
                        | FindlibPackage (nm, ver_opt) ->
                            add nm 
                              {optional        = optional;
                               version_cmp     = ver_opt;
                               package_version = None}
                              acc

                        | InternalLibrary _ ->
                            acc)
                   acc
                   bs.bs_build_depends
             end
               
         | Flag _ | Test _ | Doc _ | SrcRepo _ ->
             acc)
    MapString.empty
    pkg.sections

let fold = 
  MapString.fold 

let solve ~ctxt t = 
  (* 1. Build a map of findlib_name -> package versions *)
  ODBProvides.map ~ctxt () 
  >|= fun provides -> 

  (* 2. Solve the build dependencies *)
  begin
    MapString.mapi
      (fun fndlb_nm e ->
         if e.package_version = None then
           begin
             (* TODO: rely on ODBStorage.Ver.latest rather than
              * on List.sort
              *)
             try 
               let lst = 
                 (* All version that provides this findlib library *)
                 MapString.find fndlb_nm provides
               in
               let lst = 
                 (* Only the one that match the version constraint *)
                 match e.version_cmp with 
                   | Some cmp ->
                       List.filter 
                         (fun ver -> comparator_apply ver.ver cmp)
                         lst
                   | None ->
                       lst
               in
               let lst =
                 (* Sort the result, latest version first *)
                 List.sort (fun v1 v2 -> ~- (ODBPkgVer.compare v1 v2)) lst
               in
                 match lst with 
                   | ver :: _ ->
                       {e with package_version = Some ver}
                   | [] ->
                       e

             with Not_found ->
               e
           end
         else
           e)
      t
  end

