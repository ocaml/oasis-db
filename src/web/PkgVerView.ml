
(** Web services to display package version
    @author Sylvain Le Gall
  *)

open CalendarLib
open Lwt
open XHTML.M
open Eliom_predefmod.Xhtml
open Context
open OASISVersion
open OASISTypes
open ODBPkgVer
open ODBGettext
open Common
open Template

let email_hide = 
  let email_re = 
    Pcre.regexp "(<)?(\\S)\\S*@\\S*(\\S)(>)?"
  in
  let email_subst = 
    Pcre.subst "$1$2...@...$3$4"
  in
    fun ~ctxt str ->
      if is_admin ~ctxt () then
        str
      else
        Pcre.replace ~rex:email_re ~itempl:email_subst str

let oasis_fields ~ctxt ~sp pkg =
  (* TODO: link to packages that rev-depends
   *)

  (* Build a map of provide -> package's versions *)
  ODBProvides.map ctxt.stor
  >|= fun provides -> 

  begin
    let deps = 
      ODBDeps.solve (ODBDeps.of_oasis_package pkg) provides
    in
 
    let build_tools, build_deps =
      ODBDeps.fold
        (fun dep e (build_tools, build_deps) ->
           let scmp = string_of_comparator in
           let spf  fmt = Printf.sprintf fmt in
           let nm = 
             match dep with
               | `ExternalTool prog -> prog
               | `FindlibPackage fndlb -> fndlb
           in
           let txt = 
             match e.ODBDeps.version_cmp, e.ODBDeps.optional with 
               | Some cmp, false ->
                   spf (f_ "%s (%s)") nm (scmp cmp)
               | Some cmp, true ->
                   spf (f_ "%s (%s, opt.)") nm (scmp cmp) 
               | None, false ->
                   nm
               | None, true ->
                   spf (f_ "%s (opt.)") nm
           in
           let hd = 
             match e.ODBDeps.package_version with 
               | Some pkg_ver ->
                   a 
                     view 
                     sp 
                     [pcdata txt]
                     (pkg_ver.pkg, Version pkg_ver.ver)
               | None ->
                   pcdata txt 
           in
             match dep with 
               | `ExternalTool _ -> 
                   hd :: build_tools, 
                   build_deps
               | `FindlibPackage _ ->
                   build_tools,
                   hd :: build_deps)
        deps
        ([], [])
    in
   
    let provide_tools, provide_fndlbs =
      List.fold_left
        (fun (provide_tools, provide_fndlbs) (k, status) ->
           let nm =
             match k with 
               | `ExternalTool (nm, _)
               | `FindlibPackage (nm, _) ->
                   nm
           in
           let str =
             match status with 
               | `Always ->
                   nm
               | `Sometimes ->
                   Printf.sprintf (f_ "%s (opt.)") nm
           in
             match k with 
               | `ExternalTool _ ->
                   str :: provide_tools,
                   provide_fndlbs
               | `FindlibPackage _ ->
                   provide_tools,
                   str :: provide_fndlbs)
        ([], [])
        (ODBProvides.of_oasis_package pkg)
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
          "build_depends"
          (sn_ "Build Depend: " "Build Depends: ")
          build_deps;

        non_zero_lst_html
          "build_tools"
          (sn_ "Build Tool: " "Build Tools: ")
          build_tools;

        non_zero_lst
          "provide_tools"
          (sn_ "Provide Tool: " "Provide Tools: ")
          provide_tools;

        non_zero_lst
          "provide_findlib"
          (sn_ "Provide Findlib: " "Provide Findlibs: ")
          provide_fndlbs;

        Some 
          ("license",
           s_ "License: ",
           [pcdata (OASISLicense.to_string pkg.license)]);

        non_zero_lst
          "authors"
          (sn_ "Author: " "Authors: ") 
          (List.map (email_hide ~ctxt) pkg.authors);

        non_zero_lst
          "maintainers"
          (sn_ "Maintainer: " "Maintainers: ")
          (List.map (email_hide ~ctxt) pkg.maintainers);

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

let box ~ctxt ~sp pkg_ver backup_link oasis_opt = 
  (catch 
     (fun () ->
        ODBStorage.PkgVer.elements 
          ~extra:pkg_ver 
          ctxt.stor 
          (`PkgVer pkg_ver))
     (function 
        | Not_found ->
            return [pkg_ver]
        | e ->
            fail e))
  >>= fun pkg_ver_lst ->
  catch 
    (fun () ->
       ODBStorage.PkgVer.latest 
         ~extra:pkg_ver 
         ctxt.stor
         (`PkgVer pkg_ver))
    (function 
       | Not_found ->
           return pkg_ver
       | e ->
           fail e)
  >>= fun pkg_ver_latest ->
  backup_link () 
  >>= fun (a_backup, fn_backup) ->
  Distro.pkg_ver_box ~sp ~ctxt pkg_ver
  >>= fun distro_box ->
  Comment.pkg_ver_box ~sp ~ctxt pkg_ver
  >>= fun comment_box ->
  Rating.pkg_ver_box  ~sp ~ctxt pkg_ver
  >>= fun (rating_box, _) ->
  begin
    match oasis_opt with 
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

  let downloads =
    match pkg_ver.publink with 
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

    [
      (match description with 
         | Some txt -> 
             div 
               ~a:[a_id "description"]
               (MarkdownExt.to_html txt)
         | None -> 
             pcdata "");

      odd_even_table 
        (field 
           ("versions", (s_ "Versions: "), 
            versions_field ~sp pkg_ver_lst (Some pkg_ver) pkg_ver_latest))
        (gen_fields
           (extra_fields
            @
            [
              Some 
                ("upload_date",
                 s_ "Upload date: ",
                 [pcdata (Printer.Calendar.to_string 
                            pkg_ver.upload_date)]);

              Some 
                ("upload_method",
                 s_ "Upload method: ",
                 [pcdata (string_of_upload_method 
                            pkg_ver.upload_method)]);

              Some 
                ("downloads",
                 sn_ "Download: " "Downloads: " (List.length downloads),
                 html_concat (pcdata (s_ ", ")) downloads);

              Some
                ("distro",
                 s_ "Install: ",
                 [distro_box]);

              Some
                ("rating",
                 s_ "Rating: ",
                 [rating_box]);
            ]));

      comment_box;
    ]

let page ~ctxt ~sp pkg_ver = 
  (* Load OASIS file *)
  ODBStorage.PkgVer.oasis ctxt.stor (`PkgVer pkg_ver) 
  >>= fun oasis_opt ->

  begin
    let backup_link () = 
      (* Backup download link *)
      Dist.a_dist 
        ~sp ~ctxt pkg_ver 
        (fun fn -> 
           [pcdata 
              (Printf.sprintf 
                 (f_ "%s (backup)") 
                 (FilePath.basename fn))])
        `Tarball
    in
      box ~ctxt ~sp pkg_ver backup_link oasis_opt
  end
  >>= fun box ->
  Monitor.pkg_ver_box ~sp ~ctxt ()
  >>= fun monitor_box ->
  Rating.pkg_ver_box ~sp ~ctxt pkg_ver
  >>= fun (_, rating_box) ->
  begin
    (* Page titles *)
    let browser_ttl =
      Printf.sprintf (f_ "%s v%s") 
        pkg_ver.pkg 
        (string_of_version pkg_ver.ver)
    in
    let page_ttl = 
      match oasis_opt with 
        | Some {synopsis = s} -> 
            Printf.sprintf (f_ "%s: %s") pkg_ver.pkg s
        | None -> 
            pkg_ver.pkg
    in
      template 
        ~ctxt
        ~sp 
        ~title:(BrowserAndPageTitle (browser_ttl, page_ttl)) 
        ~div_id:"browse" 
        [
          div box;

          div 
            ~a:[a_class ["action"]]
            [
              ul 
                (li 
                   [a view sp 
                      [pcdata (s_ "Package page")] 
                      (pkg_ver.pkg, NoVersion)])
                [li [monitor_box];
                 li [rating_box]]
            ]
        ]
  end

let view_handler sp (pkg, ver_opt) () =
  Context.get ~sp () 
  >>= fun ctxt ->
  catch 
    (fun () ->
       match pkg, ver_opt with 
         | pkg_str, NoVersion ->
             ODBStorage.Pkg.find ctxt.stor (`Str pkg_str)
             >>= 
             PkgView.package_page ~ctxt ~sp 

         | pkg_str, Version ver ->
             ODBStorage.PkgVer.find ctxt.stor (`StrVer (pkg_str, ver))
             >>= 
             page ~ctxt ~sp 

         | pkg_str, LatestVersion ->
             ODBStorage.PkgVer.latest ctxt.stor (`Str pkg_str)
             >>= 
             page ~ctxt ~sp)
    (function
       | Not_found ->
           fail Eliom_common.Eliom_404
       | e ->
           fail e)
