
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

type key =
    [ `FindlibPackage of findlib_full 
    | `ExternalTool of prog]

module Map = 
  Map.Make
    (struct 
       type t = key
       let compare = Pervasives.compare
     end)

type t = elem Map.t

let add dep e1 t = 
  let e2 =
    try 
      Map.find dep t
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
    Map.add 
      dep
      {
        optional = optional;
        version_cmp = version_cmp;
        package_version = package_version;
      }
      t

(* Extract build dependencies from OASIS package *)
let of_oasis_package pkg = 
  let rec add_tool_list optional = 
    List.fold_left
      (fun acc ->
         function
           | ExternalTool nm ->
               add (`ExternalTool nm)
                 {optional        = optional;
                  version_cmp     = None;
                  package_version = None}
                 acc

           | InternalExecutable _ ->
               acc)
  in

    List.fold_left
      (fun acc ->
         function
           | Executable (_, bs, _)
           | Library (_, bs, _) ->
               begin
                 let built, optional =
                   (* If the build is optional, the dependencies
                    * are optional as well 
                    *)
                   match OASISBuildSectionExt.buildable pkg bs with 
                     | `Always -> true, false
                     | `Sometimes -> true, true
                     | `Never -> false, true
                 in

                   if built then
                     begin
                       let acc =
                         (* Add findlib packages *)
                         List.fold_left
                           (fun acc ->
                              function
                                | FindlibPackage (nm, ver_opt) ->
                                    add (`FindlibPackage nm)
                                      {optional        = optional;
                                       version_cmp     = ver_opt;
                                       package_version = None}
                                      acc

                                | InternalLibrary _ ->
                                    acc)
                           acc
                           bs.bs_build_depends
                       in

                       let acc = 
                         (* Add external tools *)
                         add_tool_list optional acc bs.bs_build_tools
                       in
                         acc
                     end
                   else
                     (* This section is not built at all *)
                     acc
               end

           | Test (cs, test) ->
               begin 
                 if OASISExprExt.trivial_choose pkg test.test_run <> Some false then
                   (* Running test is not mandatory, so it is always optional *)
                   add_tool_list true acc test.test_tools 
                 else
                   acc
               end

           | Doc (cs, doc) ->
               begin
                 if OASISExprExt.trivial_choose pkg doc.doc_build <> Some false then
                   (* Building doc is not mandatory, so it is always optional *)
                   add_tool_list true acc doc.doc_build_tools
                 else
                   acc
               end
                 
           | Flag _ | SrcRepo _ ->
               acc)
      Map.empty
      pkg.sections

let fold = 
  Map.fold 

(* Solve the build dependencies *)
let solve t provides = 
  Map.mapi
    (fun dep elt ->
       if elt.package_version = None then
         begin
           try 
             let lst = 
               (* Find package that provides this dependency *)
               ODBProvides.Map.find_name ?ver_cmp:elt.version_cmp dep provides
             in
             let lst =
               (* Sort the result, always installable version first and 
                  latest version first 
                *)
               List.sort 
                 (fun (st1, pkg_ver1) (st2, pkg_ver2) -> 
                    match st1, st2 with 
                      | `Always, `Always 
                      | `Sometimes, `Sometimes ->
                          ~- (ODBPkgVer.compare pkg_ver1 pkg_ver2)
                      | `Always, `Sometimes ->
                          -1
                      | `Sometimes, `Always ->
                          1)
                 (List.flatten lst)
             in
               match lst with 
                 | (_, pkg_ver) :: _ ->
                     {elt with package_version = Some pkg_ver}
                 | [] ->
                     elt

           with Not_found ->
             elt
         end
       else
         elt)
    t

