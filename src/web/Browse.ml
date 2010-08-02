
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open CalendarLib
open ODBVer
open OASISTypes
open ODBGettext
open Template

module BuildDepends =
struct 
  module MapString = Map.Make(String)

  include MapString

  let add nm ver1_opt t = 
    let ver2_opt =
      try 
        MapString.find nm t
      with Not_found ->
        None
    in
    let ver_opt = 
      match ver1_opt, ver2_opt with
        | Some cmp1, Some cmp2 ->
            Some 
              (OASISVersion.comparator_reduce
                 (OASISVersion.VAnd (cmp1, cmp2)))
        | None, ver_opt
        | ver_opt, None ->
            ver_opt
    in
      MapString.add nm ver_opt t

end

let browse_pkg_ver = 
  new_service 
    ~path:["browse_pkg_ver"] 
    ~get_params:(suffix (string "pkg" ** string "ver"))
    ()

let mk_version_page ~sp fver = 
  catch 
    (fun () -> 
       (fver () 
        >>= fun ver ->
        (ODBStorage.versions ver.pkg 
         >>= fun ver_lst ->
         (ODBStorage.version_latest ver.pkg
          >>= fun ver_latest ->
          return (ver, ver_lst, ver_latest))))
       >>= fun (ver, ver_lst, ver_latest) ->
       ODBStorage.version_filename 
         ver.pkg 
         (OASISVersion.string_of_version ver.ver) 
         ODBStorage.Tarball
       >>= fun tarball_fn ->
       begin
         (* Load OASIS file *)
         ODBStorage.version_filename 
           ver.pkg 
           (OASISVersion.string_of_version ver.ver)
           ODBStorage.OASIS
         >>= 
         ODBOASIS.from_file 
           (* TODO: don't use default context *)
           ~ctxt:ODBContext.default
           ~ignore_plugins:true
         >>= fun pkg ->
         return (ver, pkg)
       end
       >>= fun (ver, pkg) ->
       let browser_ttl =
         Printf.sprintf (f_ "%s v%s") 
           ver.pkg 
           (OASISVersion.string_of_version ver.ver)
       in
       let ttl = 
         Printf.sprintf (f_ "%s: %s")
           ver.pkg pkg.synopsis
       in
       let field clss (id, nm, vl) =
         (tr
            ~a:[a_id id;
                a_class [clss]]
            (th [pcdata nm]) 
            [td vl])
       in
       let rec gen_fields clss next_fields =
         function
           | Some e :: tl ->
               field clss e :: next_fields tl
           | None :: tl ->
               gen_fields clss next_fields tl
           | [] ->
               []
       and odd_fields lst = 
         gen_fields "odd" even_fields lst 
       and even_fields lst =
         gen_fields "even" odd_fields lst
       in

       let sov = 
         OASISVersion.string_of_version 
       in

       let a_of_url url =
         XHTML.M.a ~a:[a_href url]
       in

       let versions_field =
         let mk_pcdata ver = 
           if ver = ver_latest then
             b [pcdata ((sov ver.ver)^"*")]
           else
             pcdata (sov ver.ver)
         in
         let lst =
           List.fold_left
             (fun acc cur_ver ->
                let hd = 
                  if cur_ver = ver then
                    mk_pcdata cur_ver
                  else
                    a
                      (preapply browse_pkg_ver 
                         (cur_ver.pkg, sov cur_ver.ver))
                      sp
                      [mk_pcdata cur_ver]
                      ()
                in
                  hd :: acc)
             []
             ver_lst
         in
         let rec add_comma =
           function
             | e1 :: e2 :: tl ->
                 e1 :: pcdata (s_ ", ") :: add_comma (e2 :: tl)
             | _ :: [] 
             | [] as lst ->
                 lst
         in
           add_comma (List.rev lst)
       in

       let non_zero_lst id nm lst =
         let len =
           List.length lst 
         in
           if len > 0 then
             Some (id, nm len, [pcdata (String.concat (s_ ", ") lst)])
           else
             None
       in

       (* TODO: tool dependencies which includes test/doc, for dependencies
        * and provides.
        * TODO: link to packages that depends/provides 
        *)
       let dependencies =
         let deps = 
           List.fold_left
             (fun acc ->
                function
                  | Executable (_, bs, _)
                  | Library (_, bs, _) ->
                      begin
                        List.fold_left
                          (fun acc ->
                             function
                               | FindlibPackage (nm, ver_opt) ->
                                   BuildDepends.add nm ver_opt acc

                               | InternalLibrary _ ->
                                   acc)
                          acc
                          bs.bs_build_depends
                      end
                        
                  | Flag _ | Test _ | Doc _ | SrcRepo _ ->
                      acc)
             BuildDepends.empty
             pkg.sections
         in
           BuildDepends.fold
             (fun nm ver_opt acc ->
                let hd = 
                  match ver_opt with 
                    | Some cmp ->
                        Printf.sprintf (f_ "%s (%s)")
                          nm
                          (OASISVersion.string_of_comparator cmp)
                    | None ->
                        nm
                in
                  hd :: acc)
             deps
             []
       in

       let provides =
         OASISUtils.MapString.fold
           (fun _ vl acc ->
              let hd = 
                match vl with 
                  | Some pre, nm -> 
                      pre^"."^nm
                  | None, nm -> 
                      nm
              in
                hd :: acc)
           (OASISLibrary.findlib_name_map pkg)
           []
       in

         page_template sp browser_ttl Account.box
           [
             h2 [pcdata ttl];

             div 
               ~a:[a_id "browse"]
               [
                 (match pkg.description with 
                    | Some txt -> 
                        div 
                          ~a:[a_id "description"]
                          (MarkdownHTML.to_html
                             ~render_link:(fun _ -> i [pcdata "link removed"])
                             ~render_img:(fun _ -> i [pcdata "image removed"])
                             ~render_pre:(fun ~kind s -> pre [pcdata s])
                             (Markdown.parse_text txt))
                    | None -> 
                        pcdata "");

                 table 
                   (field 
                      "odd" 
                      ("versions", (s_ "Versions: "), versions_field))
                   (even_fields
                      [
                        begin
                          match pkg.homepage with
                            | Some url ->
                                Some 
                                  ("homepage",
                                   s_ "Homepage",
                                   [a_of_url 
                                      (uri_of_string url)
                                      [pcdata url]])
                            | None ->
                                None
                        end;
                        
                        non_zero_lst
                          "dependencies"
                          (sn_ "Dependency: " "Dependencies: ")
                          dependencies;

                        non_zero_lst
                          "provides"
                          (sn_ "Provide: " "Provides: ")
                          provides;

                        Some 
                          ("license",
                           s_ "License: ",
                           [pcdata (OASISLicense.to_string pkg.license)]);

                        non_zero_lst
                          "authors"
                          (sn_ "Author: " "Authors: ") 
                          pkg.authors;

                        non_zero_lst
                          "maintainers"
                          (sn_ "Maintainer: " "Maintainers: ")
                          pkg.maintainers;

                        non_zero_lst 
                          "categories"
                          (sn_ "Category: " "Categories: ")
                          pkg.categories;

                        (* TODO: VCS 
                         * s_ "Source repository: ",
                         * ...;
                         *)

                        Some 
                          ("upload_date",
                           s_ "Upload date: ",
                           [pcdata (Printer.Calendar.to_string ver.upload_date)]);

                        Some 
                          ("upload_method",
                           s_ "Upload method: ",
                           [pcdata (string_of_upload_method ver.upload_method)]);

                        Some 
                          ("downloads",
                           s_ "Downloads: ",
                           begin
                             let tarball_in_dest_dir = 
                               let pwd = 
                                 FileUtil.pwd ()
                               in
                                 FilePath.make_relative 
                                   (FilePath.make_absolute pwd ODBConf.dist_dir)
                                   (FilePath.make_absolute pwd tarball_fn)
                             in
                             let backup = 
                               (* TODO: real link *)
                               a
                                 (preapply (static_dir sp)
                                    (["dist"; tarball_in_dest_dir]))
                                 sp
                                 [pcdata (s_ "backup")]
                                 ()
                             in
                               match ver.publink with 
                                 | Some url ->
                                     (a_of_url 
                                        (uri_of_string url)
                                        [b [pcdata (s_ "upstream")]])
                                     :: backup :: []
                                 | None ->
                                     backup :: []
                           end);
                      ]);
               ]
           ])
    (function
       | Not_found ->
           fail Eliom_common.Eliom_404
       | e ->
           fail e)

let _ =
  register 
    browse_pkg_ver
    (fun sp (pkg, ver) () ->
       mk_version_page ~sp
         (fun () -> ODBStorage.version pkg ver))

let browse_pkg =
  register_new_service 
    ~path:["browse_pkg"] 
    ~get_params:(suffix (string "pkg"))
    (fun sp pkg () ->
       mk_version_page ~sp
         (fun () -> ODBStorage.version_latest pkg))


let edit_info =
  register_new_service
    ~path:["edit_info"]
    ~get_params:(suffix (string "pkg" ** string "ver"))
    (fun sp (pkg, ver) () ->
       let ttl =
         Printf.sprintf (f_ "Edit %s v%s") pkg ver
       in
         page_template sp ttl Account.box
            [h2 [pcdata ttl]])

let a_edit_info sp (pkg,ver) = 
  a (preapply edit_info (pkg, ver)) sp 
    [pcdata (s_ "Edit version")] ()

let a_browse_pkg_ver sp (pkg, ver) =
  a (preapply browse_pkg_ver (pkg, ver)) sp
    [pcdata (s_ "Browse version")] ()

let _ = 
  register
    browse
    (fun sp () () ->
      ODBStorage.packages () 
      >>= 
      Lwt_list.map_s
        (fun pkg ->
           ODBStorage.version_latest pkg 
           >>= fun ver ->
           ODBStorage.version_filename 
             ver.pkg 
             (OASISVersion.string_of_version ver.ver)
             ODBStorage.OASIS
           >>= 
           ODBOASIS.from_file 
             (* TODO: don't use default context *)
             ~ctxt:ODBContext.default
             ~ignore_plugins:true
           >>= fun oasis_pkg ->
           return (pkg, ver, oasis_pkg))
      >>= fun pkg_lst ->
      page_template sp (s_ "Browse packages") Account.box
        [
          h2 [pcdata (s_ "Packages by category")];

          (* TODO: category list + group by category *)
          
          begin
            match pkg_lst with 
            | hd :: tl ->
                begin
                  let to_li (pkg, _, oasis_pkg) = 
                    li 
                      [a (preapply browse_pkg pkg)
                        sp [pcdata pkg] ();
                       
                       pcdata 
                         (Printf.sprintf (f_ ": %s")
                            oasis_pkg.synopsis)
                      ]
                  in
                    ul (to_li hd) (List.map to_li tl)
                end
            | [] ->
                pcdata ""
          end
        ])
