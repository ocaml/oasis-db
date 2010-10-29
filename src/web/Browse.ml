
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
open Context
open Template
open Common

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

let non_zero_lst id nm lst =
  let len =
    List.length lst 
  in
    if len > 0 then
      Some (id, nm len, [pcdata (String.concat (s_ ", ") lst)])
    else
      None

let oasis_fields pkg =
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
    [
      begin
        match pkg.homepage with
          | Some url ->
              Some 
                ("homepage",
                 s_ "Homepage",
                 [XHTML.M.a 
                    ~a:[a_href (uri_of_string url)]
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
    ]


let version_page_box ~ctxt ~sp ver backup_link oasis_fn = 
  (catch 
     (fun () ->
        ODBStorage.Ver.elements ver.pkg)
     (function 
        | Not_found ->
            return [ver]
        | e ->
            fail e))
  >>= fun ver_lst ->
  (catch 
     (fun () ->
        ODBStorage.Ver.latest ver.pkg)
     (function 
        | Not_found ->
            return ver
        | e ->
            fail e))
  >>= fun ver_latest ->
  backup_link () 
  >>= fun (a_backup, fn_backup) ->

  (* Load OASIS file *)
  catch 
    (fun () ->
       oasis_fn
       >>= 
       ODBOASIS.from_file 
         ~ctxt:ctxt.odb
       >>= fun pkg ->
       return (Some pkg))
    (fun e ->
       return None)

  >|= fun pkg_opt ->

  (* End of data gathering, really create the page *)
  let synopsis, description, extra_fields = 
    match pkg_opt with 
      | Some pkg ->
          Some pkg.synopsis,
          pkg.description,
          oasis_fields pkg

      | None -> 
          None,
          (* TODO: link to create an oasis file *)
          Some (s_ "This version doesn't have an _oasis file."),
          []
  in

  let sov = 
    OASISVersion.string_of_version
  in

  (* Field generation: remove fields not set *)
  let field (id, nm, vl) =
    (tr
       ~a:[a_id id]
       (th [pcdata nm]) 
       [td vl])
  in
  let rec gen_fields =
    function
      | Some e :: tl ->
          field e :: gen_fields tl
      | None :: tl ->
          gen_fields tl
      | [] ->
          []
  in

  (* Field for versions number and their links to 
   * version's page
   *)
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
                 browse
                 sp
                 [mk_pcdata cur_ver]
                 (Some cur_ver.pkg, Some (sov cur_ver.ver))
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

    pkg_opt,
    [
      (match description with 
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

      odd_even_table 
        (field 
           ("versions", (s_ "Versions: "), versions_field))
        (gen_fields
           (extra_fields
            @
            [
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
                   match ver.publink with 
                     | Some url ->
                         [
                           (XHTML.M.a 
                              ~a:[a_href (uri_of_string url)]
                              [b 
                                 [pcdata 
                                    (FilePath.basename fn_backup)]]);
                           a_backup
                         ]
                     | None ->
                         [a_backup]
                 end);
            ]))]

let browse_version_page ~ctxt ~sp fver = 
  catch 
    (fun () -> 
       fver () 
       >>= fun ver ->
       begin
         let backup_link () = 
           (* Backup download link *)
           Dist.a_dist 
             ~sp ~ctxt ver 
             (fun fn -> 
                [pcdata 
                   (Printf.sprintf 
                      (f_ "%s (backup)") 
                      (FilePath.basename fn))])
             `Tarball
         in
         let oasis_fn = 
           ODBStorage.Ver.filename 
             ver.pkg 
             (OASISVersion.string_of_version ver.ver)
             `OASIS
         in
           version_page_box ~ctxt ~sp
             ver backup_link oasis_fn
       end
       >|= fun (oasis_pkg, content) ->
       begin
         (* Page titles *)
         let browser_ttl =
           Printf.sprintf (f_ "%s v%s") 
             ver.pkg 
             (OASISVersion.string_of_version ver.ver)
         in
         let page_ttl = 
           match oasis_pkg with 
             | Some {synopsis = s} -> 
                 Printf.sprintf (f_ "%s: %s") ver.pkg s
             | None -> 
                 ver.pkg
         in
           template 
             ~ctxt
             ~sp 
             ~title:(BrowserAndPageTitle (browser_ttl, page_ttl)) 
             ~div_id:"browse" 
             content
       end)
    (function
       | Not_found ->
           fail Eliom_common.Eliom_404
       | e ->
           fail e)

let browse_all ~ctxt ~sp () = 
  ODBStorage.Pkg.elements () 
  >>= 
  Lwt_list.map_s
    (fun pkg ->
       ODBStorage.Ver.latest pkg 
       >>= fun ver ->
       catch 
         (fun () ->
            ODBStorage.Ver.filename 
              ver.pkg 
              (OASISVersion.string_of_version ver.ver)
              `OASIS
            >>= 
            ODBOASIS.from_file 
              ~ctxt:ctxt.odb
            >>= fun oasis ->
            return (pkg, ver, Some oasis))
         (fun e ->
            return (pkg, ver, None)))
  >|= fun pkg_lst ->
  template 
    ~ctxt
    ~sp 
    ~title:(BrowserAndPageTitle (s_ "Browse", s_ "Packages by category"))
    ~div_id:"browse"
    [
      (* TODO: category list + group by category *)
      begin
        match pkg_lst with 
        | hd :: tl ->
            begin
              let to_li (pkg, _, oasis_opt) = 
                li 
                  [a browse sp [pcdata pkg] (Some pkg, None);
                   
                   match oasis_opt with 
                     | Some oasis ->
                         pcdata 
                           (Printf.sprintf (f_ ": %s")
                              oasis.synopsis)
                     | None ->
                         pcdata ""]
              in
                ul (to_li hd) (List.map to_li tl)
            end
        | [] ->
            pcdata ""
      end
    ]

(*
let edit_info =
  register_new_service
    ~path:["edit_info"]
    ~get_params:(suffix (string "pkg" ** string "ver"))
    (fun sp (pkg, ver) () ->
       let ttl =
         Printf.sprintf (f_ "Edit %s v%s") pkg ver
       in
         auth_template ~sp ~title:(OneTitle ttl) ~div_id:"edit_info" ()
         >>= fun (_, tmpl, _) ->
           tmpl [])
 *)

let browse_handler sp (pkg_opt, ver_opt) () =
  Context.get ~sp () 
  >>= fun ctxt ->
  begin
    match pkg_opt, ver_opt with 
      | Some pkg, Some ver ->
          browse_version_page ~ctxt ~sp 
            (fun () -> ODBStorage.Ver.find pkg ver)

      | Some pkg, None ->
          browse_version_page ~ctxt ~sp
            (fun () -> ODBStorage.Ver.latest pkg)

      | None, None ->
          browse_all ~ctxt ~sp ()

      | None, Some _ ->
          fail Eliom_common.Eliom_404
  end

let init server = 
  ()

