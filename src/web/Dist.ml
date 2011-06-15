
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

let mk_pkg_ver_fn ~ctxt fn_t pkg_ver = 
  ODBStorage.PkgVer.file_exists ctxt.stor (`PkgVer pkg_ver) fn_t
  >>= fun exists ->
  ODBStorage.PkgVer.filename ctxt.stor (`PkgVer pkg_ver) fn_t
  >>= fun fn ->
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

             | [pkg_str] ->
                 begin
                   (* Access to a package  -> list version + package files *)
                   ODBStorage.PkgVer.elements ctxt.stor (`Str pkg_str)
                   >|= fun lst ->
                   ("dir", "download", [pkg_str; "download"])
                   ::
                   (List.map 
                      (fun pkg_ver -> 
                         let ver_str = 
                           OASISVersion.string_of_version
                             pkg_ver.ver
                         in
                           "dir",
                           ver_str,
                           [pkg_str; ver_str])
                      lst)
                 end

             | [pkg_str; "download"] ->
                 begin
                   ODBStorage.PkgVer.elements ctxt.stor (`Str pkg_str)
                   >>= 
                   Lwt_list.map_s (mk_pkg_ver_fn ~ctxt `Tarball)
                 end

             | [pkg_str; ver_str] ->
                 (* Access to a package's version file *)
                 begin
                   ODBStorage.PkgVer.find ctxt.stor (`Str (pkg_str, ver_str))
                   >>= fun pkg_ver ->
                   Lwt_list.fold_left_s
                     (fun acc fn_t ->
                        catch 
                          (fun () ->
                             (mk_pkg_ver_fn ~ctxt fn_t pkg_ver)
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
                 String.compare nm1 nm2)
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

         catch 
           (fun () ->
              let fs = 
                ODBStorage.fs ctxt.stor
              in
              let fn =
                FilePath.make_filename lst
              in
                fs#file_exists fn
                >>= fun file_exists ->
                if file_exists then
                  begin
                    fs#is_directory fn 
                    >>= fun is_directory ->
                    if not is_directory then
                      begin
                        ODBVFS.with_file_in fs fn 
                          (fun chn ->
                             LwtExt.IO.with_file_content_chn chn) 
                        >>= fun str ->
                        (* TODO: use the right mime type *)
                        Text.send ~sp (str, "application/octet-stream")
                      end
                    else
                      generate_index lst 
                  end
                else
                  begin
                    generate_index lst
                  end)

           (function
              | ODBFSDisk.Not_subdir _ ->
                  fail Eliom_common.Eliom_404
              | e ->
                  fail e))

let a_dist ~sp ~ctxt pkg_ver fcontent fn_t = 
  mk_pkg_ver_fn ~ctxt fn_t pkg_ver
  >|= fun (_, nm, path) ->
  Xhtml.a dist sp (fcontent nm) path, nm

