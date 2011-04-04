
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
    ODBStorage.PkgVer.filename ctxt.stor pkg ver_str fn_t
    >>= fun fn ->
    ctxt.stor.ODBStorage.fs#file_exists fn
    >>= fun exists ->
      let path =
        (* TODO: create something in FilePath to handle 
         * this 
         *)
        ExtString.String.nsplit fn "/"
      in
      let bn = 
        FilePath.basename fn
      in
      let classe = 
        match fn_t with 
          | `Tarball -> "tarball"
          | `OASIS | `OASISPristine -> "oasis"
          | `PluginData _ -> "sexp"
          | `Other _ -> "other"
      in
        if exists then
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
                   ODBStorage.Pkg.elements ctxt.stor
                   >|= 
                   (List.map 
                      (fun {ODBPkg.pkg_name = pkg_str} ->
                         "dir", pkg_str, [pkg_str]))
                 end

             | [pkg] ->
                 begin
                   (* Access to a package  -> list version + package files *)
                   ODBStorage.PkgVer.elements ctxt.stor pkg
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
                   ODBStorage.PkgVer.elements ctxt.stor pkg
                   >>= 
                   Lwt_list.map_s (mk_ver_fn ~ctxt `Tarball)
                 end

             | [pkg; ver_str] ->
                 (* Access to a package's version file *)
                 begin
                   ODBStorage.PkgVer.find ctxt.stor pkg ver_str
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
         template 
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
           ]
         >>= fun page ->
         Xhtml.send ~sp page
       in

         try 
           begin
             let fn = 
               ctxt.stor.ODBStorage.fs#rebase (FilePath.make_filename lst)
             in
               if Sys.file_exists fn && not (Sys.is_directory fn) then
                 Files.send ~sp fn
               else
                 generate_index lst 
           end

         with ODBFilesystem.NotSubdir _ ->
           begin
             fail Eliom_common.Eliom_404
           end)

let a_dist ~sp ~ctxt ver fcontent fn_t = 
  mk_ver_fn ~ctxt fn_t ver
  >|= fun (_, nm, path) ->
  Xhtml.a dist sp (fcontent nm) path, nm

