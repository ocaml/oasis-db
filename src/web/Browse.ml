
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open CalendarLib
open ODBPkgVer
open OASISTypes
open OASISVersion
open ODBGettext
open Context
open Template
open Common

module MapStringCsl = 
  Map.Make 
    (struct 
       type t = string option 

       let compare t1 t2 = 
         match t1, t2 with 
           | None, None ->
               0
           | Some _, None ->
               1
           | None, Some _ ->
               -1
           | Some s1, Some s2 ->
               OASISUtils.compare_csl s2 s1
     end)

let add_acc cat data mp =
  let prev = 
    try
      MapStringCsl.find cat mp
    with Not_found ->
      []
  in
    MapStringCsl.add cat (data :: prev) mp

let browse_topics = 
  new_service 
    ~path:["browse_topics"]
    ~get_params:unit
    ()

let browse_kinds = 
  new_service
    ~path:["browse_kinds"]
    ~get_params:unit
    ()

let browse_authors = 
  new_service
    ~path:["browse_authors"]
    ~get_params:unit
    ()

let browse_any ~ctxt ~sp service ttl cat_name cat_none classify () = 
  ODBStorage.Pkg.elements ctxt.stor 
  >>= 
  Lwt_list.fold_left_s
    (fun acc ({ODBPkg.pkg_name = pkg_str} as pkg) ->
       catch 
         (fun () ->
            ODBStorage.PkgVer.latest ctxt.stor (`Pkg pkg)
            >>= fun pkg_ver ->
            ODBStorage.PkgVer.oasis ctxt.stor (`PkgVer pkg_ver)
            >|= fun oasis_opt -> 
            (pkg_str, pkg_ver, oasis_opt) :: acc)
         (function
            | Not_found ->
                return acc
            | e ->
                fail e))
    []
  >>= fun pkg_lst ->

  (* Build a map after extracting categories from package version *)
  Lwt_list.fold_left_s
    (fun mp ((pkg_str, ver, oasis_opt) as data) ->
       classify pkg_str ver oasis_opt
       >|= function
         | [] ->
             add_acc None data mp
         | cats ->
             List.fold_left
               (fun mp cat -> add_acc (Some cat) data mp)
               mp cats)
    MapStringCsl.empty
    pkg_lst
  >>= fun categories ->

  begin

    let one_category cat lst (toc, sections) = 
      let cat = 
        match cat with 
          | Some nm -> nm
          | None -> cat_none
      in
      let anchor = 
        "cat:"^cat
      in

      let to_li (pkg, _, oasis_opt) = 
        li 
          [a view sp [pcdata pkg] (pkg, LatestVersion);
           
           match oasis_opt with 
             | Some oasis ->
                 pcdata 
                   (Printf.sprintf (f_ ": %s")
                      oasis.synopsis)
             | None ->
                 pcdata ""]
      in

      let toc_item =
        [XHTML.M.a 
           ~a:[a_href (uri_of_string ("#"^anchor))]
              [pcdata (s_ cat)]; 
            pcdata (Printf.sprintf " (%d)" (List.length lst))]
      in

      let section =
        [h3 
           ~a:[a_class ["category"]]
           [XHTML.M.a
              ~a:[a_id anchor]
              [pcdata cat]];

         begin
           match lst with
             | hd :: tl -> 
                 ul (to_li hd) (List.map to_li tl)
             | [] -> 
                 pcdata (s_ "Nothing")
         end]
      in
        toc_item :: toc,
        section :: sections
    in

    let toc, sections = 
      MapStringCsl.fold
        one_category 
        categories
        ([], [])
    in

    let toc = 
      html_flatten (pcdata (s_ ", ")) toc
    in
    let sections = 
      html_flatten (pcdata "") sections 
    in

      template 
        ~ctxt
        ~sp 
        ~title:(BrowserAndPageTitle (s_ "Browse", ttl))
        ~div_id:"browse"
        ((Eliom_tools.menu ~service ~sp ~id:"submenu" 
            (browse_topics,  [pcdata (s_ "By topics")])
            [browse_kinds,   [pcdata (s_ "By kinds")];
             browse_authors, [pcdata (s_ "By authors")]])
        ::
         p ~a:[a_class ["toc"]]
           ((pcdata (Printf.sprintf (f_ "%s: ") cat_name))
           :: toc)
        :: 
         sections)
  end

let browse_topics_handler ~ctxt ~sp () = 
  browse_any ~ctxt ~sp 
    browse_topics
    (s_ "Packages by topic")
    (s_ "Topics")
    (s_ "Unclassified")
    (fun pkg ver oasis_opt ->
       return [])
    ()


let browse_kinds_handler ~ctxt ~sp () = 
  browse_any ~ctxt ~sp 
    browse_kinds
    (s_ "Packages by kind")
    (s_ "Kinds")
    (s_ "Unclassified")
    (fun pkg ver oasis_opt ->
       return [])
    ()

let email_strip = 
  let email_re = 
    Pcre.regexp " ?<.*>$"
  in
    Pcre.substitute_substrings
      ~rex:email_re
      ~subst:(fun _ -> "")

let browse_authors_handler ~ctxt ~sp () = 
  browse_any ~ctxt ~sp 
    browse_authors
    (s_ "Package by author")
    (s_ "Authors")
    (s_ "No authors")
    (fun pkg ver ->
       function 
         | Some {authors = lst} ->
             let one_author str = 
               (* Email stripping *)
               email_strip str
             in
               return (List.map one_author lst)
         | None ->
             return [])
    ()

let () = 
  let register_with_context (srvc, hdlr) = 
    register
      srvc
      (fun sp () () ->
         Context.get ~sp () 
         >>= fun ctxt ->
         hdlr ~ctxt ~sp ())
  in
    List.iter 
      register_with_context
      [browse_topics,  browse_topics_handler;
       browse_kinds,   browse_kinds_handler;
       browse_authors, browse_authors_handler]

let browse_handler sp () () = 
  Context.get ~sp () 
  >>= fun ctxt ->
  browse_topics_handler ~ctxt ~sp ()
