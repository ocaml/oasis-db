
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open ODBGettext

(* Main services *)
let home = 
  new_service ["home"] unit ()

let browse =
  new_service ["browse"] unit ()

let upload = 
  new_service ["upload"] unit () 

let contribute = 
  new_service ["contribute"] unit ()

let about =
  new_service ["about"] unit ()

let mk_static_uri sp path =
  make_uri
    ~service:(static_dir sp) 
    ~sp
    path

let page_template sp ttl account_box content =
  let mk_static_uri path =
    mk_static_uri sp path
  in
  let mk_sponsor nm url logo = 
    let url =
      uri_of_string url
    in
    let logo =
      mk_static_uri logo
    in
      XHTML.M.a ~a:[a_href url]
        [pcdata nm],
      XHTML.M.a ~a:[a_href url]
        [img ~alt:nm ~src:logo ()]
  in

  let ocamlcore_link, ocamlcore_logo  = 
    mk_sponsor
      "OCamlCore"
      "http://www.ocamlcore.com"
      ["ocamlcore-logo_badge.png"]
  in

  let janest_link, janest_logo = 
    mk_sponsor
      "Jane Street Capital"
      "http://www.janestreet.com"
      ["jane-street-logo_badge.png"]
  in

    (account_box sp)
    >>=
    (fun account_box ->
       return 
         (html 
            (head 
               (title 
                  (pcdata 
                     (Printf.sprintf "OASIS DB: %s" ttl)))
               [
                 link ~a:[a_rel [`Stylesheet];
                          a_href (mk_static_uri ["default.css"]);
                          a_type "text/css"] ();

                 link ~a:[a_rel [`Other "shortcut icon"];
                          a_href (mk_static_uri ["caml.ico"]);
                          a_type "images/ico"] ();

                 (* TODO: RSS feed *)
               ])
            (body 
               [div ~a:[a_id "top"]
                  [div ~a:[a_id "header"]
                     [h1 
                        [a home sp [pcdata "OASIS DB"] ()];
                      div ~a:[a_id "subtitle"]
                        [pcdata (s_ "an OCaml packages archive")]];
                   
                   div ~a:[a_id "account"] account_box;

                   div ~a:[a_id "menu"]
                     [ul
                        (li [a home sp       [pcdata (s_ "Home")] ()])
                        [li [a browse sp     [pcdata (s_ "Browse")] ()];
                         li [a upload sp     [pcdata (s_ "Upload")] ()];
                         li [a contribute sp [pcdata (s_ "Contribute")] ()]]];

                   div ~a:[a_id "content"]
                     content;

                   div ~a:[a_id "footer"]
                     [
                       (div ~a:[a_class ["copyright"]]
                          [
                            pcdata (s_ "(C) Copyright 2010, ");
                            ocamlcore_link;
                            pcdata ", ";
                            janest_link
                          ]);

                       (div ~a:[a_class ["sponsors"]]
                          [pcdata (s_ "Sponsors ");
                           ocamlcore_logo; 
                           janest_logo]);

                       (div ~a:[a_class ["links"]]
                          [a about sp [pcdata (s_ "About this website")] ()]);
                     ]]])))

let _ =
  (* Empty account box for errors *)
  let _account_box _ = 
    []
  in

  let error_template ?code ~sp lst = 
    (* TODO: use the real big template *)
    Eliom_predefmod.Xhtml.send ?code ~sp
      (html
         (head (title (pcdata "OASIS DB error")) [])
         (body (h1 [pcdata "OASIS DB error"] :: lst)))
  in

  let backtrace acc =
    if Printexc.backtrace_status () then 
      p [pcdata (Printexc.get_backtrace ())] :: acc
    else
      acc
  in

  Eliom_services.set_exn_handler
    (fun sp e -> 
       match e with
         | Eliom_common.Eliom_404 as e ->
             raise e

         | Eliom_common.Eliom_Wrong_parameter ->
             error_template ~sp [p [pcdata "Wrong parameters"]]

         | Failure str ->
             error_template ~sp (backtrace [p [pcdata str]])

         | e -> 
             error_template ~sp
               (backtrace 
                [p [pcdata (Printexc.to_string e)]]))
