
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open CalendarLib
open ODBVer
open OASISTypes
open OASISVersion
open ODBGettext
open Context
open Template
open Common

let rec html_flatten sep =
  function
    | e1 :: ((_ :: _) as tl) ->
        e1 @ (sep :: (html_flatten sep tl))
    | [e] ->
        e
    | [] ->
        []

let rec html_concat sep =
  function
    | e1 :: ((_ :: _) as tl) ->
        e1 :: sep :: (html_concat sep tl)
    | [_] | [] as lst ->
        lst

let non_zero_lst_html id nm lst = 
  let len =
    List.length lst 
  in
    if len > 0 then
      Some (id, nm len, html_concat (pcdata (s_ ", ")) lst)
    else
      None

let non_zero_lst id nm lst =
  non_zero_lst_html id nm (List.map pcdata lst)

let oasis_fields ~ctxt ~sp pkg =
  (* TODO: link to packages that rev-depends
   *)

  (* Compute dependencies *)
  ODBDeps.solve ~ctxt:ctxt.odb (ODBDeps.of_oasis_package pkg)
  >|= fun deps ->

  begin
    let dependencies =
      ODBDeps.fold
        (fun nm e acc ->
           let scmp = string_of_comparator in
           let spf  fmt = Printf.sprintf fmt in
           let txt = 
             match e.ODBDeps.version_cmp, e.ODBDeps.optional with 
               | Some cmp, false ->
                   spf (f_ "%s (%s)") nm (scmp cmp)
               | Some cmp, true ->
                   spf (f_ "%s (%s, optional)") nm (scmp cmp) 
               | None, false ->
                   nm
               | None, true ->
                   spf (f_ "%s (optional)") nm
           in
           let hd = 
             match e.ODBDeps.package_version with 
               | Some ver ->
                   a 
                     browse 
                     sp 
                     [pcdata txt]
                     (Some ver.pkg, Some ver.ver)
               | None ->
                   pcdata txt 
           in
             hd :: acc)
        deps
        []
    in
   
    let provides =
      ODBProvides.of_oasis_package pkg
    in

    let src_repos = 
      let one_source_repo cs repo = 
        let a_location = 
          XHTML.M.a 
            ~a:[a_href (uri_of_string repo.src_repo_location)]
            [pcdata repo.src_repo_location]
        in

        let the err f = 
          function 
            | Some e ->
                f e
            | None ->
                span ~a:[a_class ["error"]] err
        in

        let vcs_get = 
          match repo.src_repo_type with 
            | Darcs ->
                begin
                  let tag_opts = 
                    match repo.src_repo_tag with 
                      | Some tag ->
                          Printf.sprintf "-t '%s'"
                            (Pcre.quote tag)
                      | None ->
                          ""
                  in
                    [pcdata (Printf.sprintf "darcs get %s" tag_opts); 
                     a_location]
                end

            | Git ->
                begin
                  let branch_opt = 
                    match repo.src_repo_branch with 
                      | Some b -> Printf.sprintf "-b %s" b
                      | None   -> ""
                  in

                  let command = 
                    [pcdata ("git clone "^branch_opt); a_location]
                  in

                    match repo.src_repo_tag with 
                      | Some tag ->
                          begin
                            let dn = 
                              OASISUnixPath.basename repo.src_repo_location
                            in
                            let dn = 
                              if ExtString.String.ends_with dn ".git" then
                                String.sub dn 0 ((String.length dn) - (String.length ".git"))
                              else
                                dn
                            in
                              command @
                              [pcdata 
                                 (Printf.sprintf 
                                    " && cd %s && git checkout %s"
                                    dn tag)]
                          end
                      | None ->
                          command
                end

            | Svn ->
                begin
                  let a_location = 
                    match repo.src_repo_subdir with
                      | Some dn ->
                          let url = 
                            OASISUnixPath.concat repo.src_repo_location dn
                          in
                            XHTML.M.a
                              ~a:[a_href (uri_of_string url)]
                              [pcdata url]
                      | None ->
                          a_location
                  in
                    [pcdata "svn checkout "; a_location]
                end

            | Cvs ->
                begin
                  let tag_branch_opt = 
                    match repo.src_repo_branch, repo.src_repo_tag with 
                      | Some t, None 
                      | None, Some t ->
                          "-r "^t
                      | Some _, Some t ->
                          (* TODO: error *)
                          "-r "^t
                      | None, None ->
                          ""
                  in
                    [pcdata "cvs checkout -d "; a_location; 
                     pcdata " "; 
                     pcdata tag_branch_opt;
                     the 
                       [pcdata (s_ "No CVS module defined")]
                       pcdata repo.src_repo_module]
                end

            | Hg ->
                begin
                  let tag_branch_opt = 
                    match repo.src_repo_branch, repo.src_repo_tag with 
                      | Some t, None 
                      | None, Some t ->
                          "-u "^t
                      | Some _, Some t ->
                          (* TODO: error *)
                          "-u "^t
                      | None, None ->
                          ""
                  in
                    [pcdata ("hg clone "^tag_branch_opt); a_location]
                end

            | Bzr ->
                begin
                  let tag_opt = 
                    match repo.src_repo_tag with 
                      | Some t -> "-r tag:"^t
                      | None   -> ""
                  in
                    
                    [pcdata ("bzr branch "^tag_opt); a_location]
                end

            | Arch (* TODO *)
            | Monotone (* TODO *)
            | OtherVCS _ ->
                [pcdata "Unsupported VCS"]
        in

        let vcs_get =
          match repo.src_repo_browser with 
            | Some url ->
                vcs_get 
                @ 
                [pcdata (s_ " (");
                 XHTML.M.a
                   ~a:[a_href (uri_of_string url)]
                   [pcdata (s_ "browse")];
                 pcdata (s_ ")")]
            | None ->
                vcs_get
        in

          (cs.cs_name, vcs_get) 
      in

      let lst = 
        List.fold_left 
          (fun acc ->
             function
               | SrcRepo (cs, repo) ->
                   (one_source_repo cs repo) :: acc

               | Library _ | Executable _ | Flag _ | Test _ | Doc _ ->
                   acc)
          []
          pkg.sections
      in
        match lst with 
          | [_, html] ->
              [html]
          | lst ->
              List.rev_map 
                (fun (nm, html) ->
                   (pcdata (Printf.sprintf (f_ "(%s) ") nm)) :: html)
                lst
    in

      [
        begin
          match pkg.homepage with
            | Some url ->
                Some 
                  ("homepage",
                   s_ "Homepage: ",
                   [XHTML.M.a 
                      ~a:[a_href (uri_of_string url)]
                      [pcdata url]])
            | None ->
                None
        end;
        
        non_zero_lst_html
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

        begin
          match src_repos with 
            | [] -> None
            | lst ->
                Some
                  ("source_repositories",
                   sn_ 
                     "Source repository: " 
                     "Source repositories: " 
                     (List.length lst),
                   html_flatten (pcdata "; ") lst)
        end;
      ]
  end


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

  >>= fun pkg_opt ->

  begin
    match pkg_opt with 
      | Some pkg ->
          oasis_fields ~ctxt ~sp pkg
          >|= fun extra_fields ->
          Some pkg.synopsis,
          pkg.description,
          extra_fields

      | None -> 
          return
            (None,
             Some (s_ "This version doesn't have an _oasis file."),
             [])
  end 
  >|= fun (synopsis, description, extra_fields) ->
  (* End of data gathering, really create the page *)

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
        b [pcdata ((string_of_version ver.ver)^"*")]
      else
        pcdata (string_of_version ver.ver)
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
                 (Some cur_ver.pkg, Some cur_ver.ver)
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

  let downloads =
    match ver.publink with 
      | Some url ->
          [
            XHTML.M.a 
              ~a:[a_href (uri_of_string url)]
              [b [pcdata (FilePath.basename fn_backup)]];
              a_backup
          ]
      | None ->
          [a_backup]
  in

    pkg_opt,
    [
      (match description with 
         | Some txt -> 
             div 
               ~a:[a_id "description"]
               (MarkdownHTML.to_html
                  ~render_link:
                  (fun href -> 
                     XHTML.M.a
                       ~a:[a_href (uri_of_string href.Markdown.href_target)]
                       [pcdata href.Markdown.href_desc])
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
                 sn_ "Download: " "Downloads: " (List.length downloads),
                 html_concat (pcdata (s_ ", ")) downloads);
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
             (string_of_version ver.ver)
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
             (string_of_version ver.ver)
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
              (string_of_version ver.ver)
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
            (fun () -> ODBStorage.Ver.find pkg 
                         (string_of_version ver))

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

