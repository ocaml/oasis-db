
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open BocageGettext
open Template

(* Packages *)
let packages = 
  [
    "ocaml-fileutils", "0.4.4", "2010/05/05";
    "ocaml-gettext",   "0.5.1", "2010/05/04";
    "sexplib310",      "6.0",   "2010/05/03";
    "ocaml-pcre",      "5.1",   "2010/05/02";
  ]

let _ = 
  register
    home
    (fun sp () () ->
       page_template sp (s_ "Home") Account.box
         [
           div ~a:[a_id "home_highlight"]
             [
               ul
                 (li [a browse sp [pcdata (s_ "Browse packages")] ()])
                 [li [a browse sp [pcdata (s_ "Upload your package")] ()]];
             ];

           div ~a:[a_class ["introduction"]]
             [
               h2 [pcdata "OASIS DB, the comprehensive OCaml package archive"];
               p [pcdata 
                    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                     Aliquam eleifend consequat sem.Lorem ipsum dolor sit amet, 
                     consectetur adipiscing elit. Aliquam eleifend consequat sem. 
                     Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                     Aliquam eleifend consequat sem.Lorem ipsum dolor sit amet, 
                     consectetur adipiscing elit."];

               p [pcdata 
                    "Aliquam eleifend consequat sem.Lorem ipsum dolor sit amet, 
                     consectetur adipiscing elit. Aliquam eleifend consequat sem. 
                     Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                     Aliquam eleifend consequat sem. Lorem ipsum dolor sit amet, 
                     consectetur adipiscing elit. Aliquam eleifend consequat sem.
                     Lorem ipsum dolor sit amet, consectetur adipiscing elit."];
             ];

           div ~a:[a_class ["whatsnew"]]
             [
               h2 [pcdata "What's new"];

               (match packages with
                  | hd :: tl ->
                      begin
                        let to_li (nm, ver, date) =
                          li 
                            [a 
                               (preapply Account.browse_pkg_ver (nm, ver))
                               sp
                               [pcdata 
                                  (Printf.sprintf 
                                     (f_ "%s v%s (%s)")
                                     nm ver date)]
                            ()]
                        in
                          ul (to_li hd) (List.map to_li tl)
                      end
                  | [] ->
                      pcdata "");
             ];

           div ~a:[a_class ["statistics"]]
             [
               h2 [pcdata "Statistics"];
               img 
                 ~alt:"Package uploads graph"
                 ~src:(mk_static_uri sp ["chart-upload.png"])
                 ();
               p [pcdata "250 uploads since 2010/10/01"];
               p [pcdata "100 packages in the database"];
             ];
         ])

let _ = 
  register
    browse
    (fun sp () () ->
       page_template sp (s_ "Browse packages") Account.box
         [p [pcdata "Browse"]])

let _ = 
  register 
    upload
    (fun sp () () ->
       page_template sp (s_ "Upload packages") Account.box 
         [p [pcdata "Upload"]])

let _ = 
  register 
    contribute
    (fun sp () () ->
       page_template sp (s_ "Contribute") Account.box 
         [p [pcdata "Contribute"]])

let _ = 
  register 
    about
    (fun sp () () ->
       page_template sp (s_ "About this website") Account.box
         [p [pcdata "About"]])
