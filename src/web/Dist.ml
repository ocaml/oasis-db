
(** Access to ODBStorage dist directory
    @author Sylvain Le Gall
  *)

open Lwt
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod
open XHTML.M
open ODBPkgVer
open ODBContext
open ODBGettext
open Common
open Template
open Context

(* TODO: register temporary file *)

let dist = 
  new_service 
    ["dist"]
    (suffix (all_suffix "path"))
    ()

let mk_ver_fn ~ctxt fn_t ver = 
  let pkg = 
    ver.pkg
  in
  let ver_str = 
    OASISVersion.string_of_version
      ver.ver
  in
    ODBStorage.PkgVer.filename pkg ver_str fn_t
    >>= fun fn ->
      let pwd =
        FileUtil.pwd ()
      in
      let fn' =
        FilePath.make_relative 
          (FilePath.make_absolute pwd ctxt.odb.dist_dir)
          (FilePath.make_absolute pwd fn)
      in
      let path =
        (* TODO: create something in FilePath to handle 
         * this 
         *)
        ExtString.String.nsplit fn' "/"
      in
      let bn = 
        FilePath.basename fn'
      in
      let classe = 
        match fn_t with 
          | `Tarball -> "tarball"
          | `OASIS | `OASISPristine -> "oasis"
          | `PluginData _ -> "sexp"
          | `Other _ -> "other"
      in
        if Sys.file_exists fn then
          return (classe, bn, path)
        else
          fail Not_found 

let () = 
  Any.register 
    dist
    (fun sp lst () ->
       Context.get ~sp ()
       >>= fun ctxt ->
       let generate_index lst = 
         begin
           match lst with 
             | [] ->
                 begin
                   (* Access to / -> return list of package + top files *)
                   ODBStorage.Pkg.elements () 
                   >|= 
                   (List.map 
                      (fun pkg ->
                         "dir",
                         pkg,
                         [pkg]))
                 end

             | [pkg] ->
                 begin
                   (* Access to a package  -> list version + package files *)
                   ODBStorage.PkgVer.elements pkg
                   >|= fun lst ->
                   ("dir", "download", [pkg; "download"])
                   ::
                   (List.map 
                      (fun ver -> 
                         let ver_str = 
                           OASISVersion.string_of_version
                             ver.ver
                         in
                           "dir",
                           ver_str,
                           [pkg; ver_str])
                      lst)
                 end

             | [pkg; "download"] ->
                 begin
                   ODBStorage.PkgVer.elements pkg
                   >>= 
                   Lwt_list.map_s (mk_ver_fn ~ctxt `Tarball)
                 end

             | [pkg; ver_str] ->
                 (* Access to a package's version file *)
                 begin
                   ODBStorage.PkgVer.find pkg ver_str
                   >>= fun ver ->
                   Lwt_list.fold_left_s
                     (fun acc fn_t ->
                        catch 
                          (fun () ->
                             (mk_ver_fn ~ctxt fn_t ver)
                             >>= fun e ->
                             return (e :: acc))

                          (function 
                             | Not_found ->
                                 return acc
                             | e ->
                                 fail e))
                     []
                     [`OASIS; 
                      `OASISPristine; 
                      `Tarball]
                 end

             | _ ->
                 (* TODO: log *)
                 fail Eliom_common.Eliom_404
         end

         >>= fun lst' ->
         return
           (List.sort 
              (fun (_, nm1, _) (_, nm2, _) -> 
                 OASISVersion.version_compare 
                   (OASISVersion.version_of_string nm1)
                   (OASISVersion.version_of_string nm2))
              lst')

         >>= fun lst' ->
         Xhtml.send ~sp
           (template 
              ~ctxt
              ~sp
              ~title:(BrowserAndPageTitle 
                        (s_ "Dist", 
                         Printf.sprintf
                           (f_ "Index of /%s")
                           (String.concat "/" lst)))
              ~div_id:"dist"
              [
                table
                  (tr
                     (th [pcdata (s_ "Name")])
                     [])
                  (List.map
                     (fun (classe, nm, path) ->
                        tr 
                          (td 
                             ~a:[a_class [classe]]
                             [Xhtml.a dist sp [pcdata nm] path])
                          [])

                     (match List.rev lst with 
                        | _ :: tl ->
                            (* We have an upper dir *)
                            ("updir", "..", List.rev tl) :: lst'
                        | [] ->
                            lst'))
              ])
       in

       let fn = 
         FilePath.make_filename 
           (ctxt.odb.dist_dir :: lst)
       in

         if FilePath.is_subdir ctxt.odb.dist_dir fn then
           begin
             fail Eliom_common.Eliom_404
           end

         else
           begin
             let fn_abs = 
               FilePath.make_absolute (FileUtil.pwd ()) fn
             in
               if Sys.file_exists fn_abs && not (Sys.is_directory fn_abs) then
                 Files.send ~sp fn_abs
               else
                 generate_index lst 
           end)

let a_dist ~sp ~ctxt ver fcontent fn_t = 
  mk_ver_fn ~ctxt fn_t ver
  >|= fun (_, nm, path) ->
  Xhtml.a dist sp (fcontent nm) path, nm

