
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open ODBGettext
open Context
open Account

(* Main services *)
let home       = Defer.new_service ["home"] unit
let browse     = Defer.new_service ["browse"] unit
let upload     = Defer.new_service ["upload"] unit
let contribute = Defer.new_service ["contribute"] unit 
let about      = Defer.new_service ["about"] unit

let mk_static_uri sp path =
  make_uri
    ~service:(static_dir sp) 
    ~sp
    path

type title =
  | OneTitle of string 
  | BrowserAndPageTitle of string * string 

exception PageRequiresAuth

let template_skeleton ~sp ~title ?(extra_headers=[]) ~div_id account_box ctnt = 
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

  let browser_ttl, page_ttl =
    match title with
      | OneTitle ttl -> ttl, ttl
      | BrowserAndPageTitle (bttl, pttl) -> bttl, pttl
  in
    html 
      (head 
         (XHTML.M.title 
            (pcdata 
               (Printf.sprintf "OASIS DB: %s" browser_ttl)))
         (link ~a:[a_rel [`Stylesheet];
                   a_href (mk_static_uri ["default.css"]);
                   a_type "text/css"] ()
          ::
          link ~a:[a_rel [`Other "shortcut icon"];
                   a_href (mk_static_uri ["caml.ico"]);
                   a_type "images/ico"] ()
          ::
          extra_headers))
      (body 
         [div ~a:[a_id "top"]
            [div ~a:[a_id "header"]
               [div ~a:[a_id "title"]
                  [a (home ()) sp [pcdata "OASIS DB"] ()];
                div ~a:[a_id "subtitle"]
                  [pcdata (s_ "an OCaml packages archive")]];
             
             div ~a:[a_id "account"] account_box;

             div ~a:[a_id "menu"]
             (*
               [Eliom_tools.menu
                  ~id:"menu"
                  (home (), [pcdata (s_ "Home")])
                  [
                    browse (),     [pcdata (s_ "Browse")];
                    upload (),     [pcdata (s_ "Upload")];
                    contribute (), [pcdata (s_ "Contribute")];
                  ]
                  ~sp ()];
              *)
               [ul
                  (li [a (home ()) sp       [pcdata (s_ "Home")] ()])
                  [li [a (browse ()) sp     [pcdata (s_ "Browse")] ()];
                   li [a (upload ()) sp     [pcdata (s_ "Upload")] ()];
                   li [a (contribute ()) sp [pcdata (s_ "Contribute")] ()]]];

             if ODBConf.dev then
               div ~a:[a_id "dev_warning"]
                 [
                   p [pcdata 
                        (Printf.sprintf
                           (f_ "This is the development version %s of \
                             OASIS-DB. Packages uploaded here will be \
                             removed. Use this website to test your \
                             packages or help the OASIS-DB project to \
                             find bugs.")
                           ODBConf.version)];
                   XHTML.M.a
                     ~a:[a_href (uri_of_string "https://forge.ocamlcore.org/tracker/?group_id=54")]
                     [pcdata (s_ "Submit bug reports or feature requests")];
                   pcdata " | ";
                   XHTML.M.a
                     ~a:[a_href (uri_of_string "http://oasis.ocamlcore.org/")]
                     [pcdata (s_ "Go to the real OASIS-DB website")];
                 ]
             else
               pcdata "";

             div ~a:[a_id "content"]
               [div ~a:[a_id div_id]
                  ((h1 [pcdata page_ttl]) :: ctnt)];

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
                    [a (about ()) sp [pcdata (s_ "About this website")] ()]);
               ]]])

let unauth_template ~sp ?extra_headers ~title ~div_id () =
  Context.get ~sp () 
  >>= fun ctxt ->
  (Account.box ~role:ctxt.role sp)
  >>= fun account_box ->
  return 
    (ctxt, 
     fun ctnt -> 
       return 
         (template_skeleton 
            ~sp ?extra_headers ~title ~div_id 
            account_box ctnt))

let auth_template ~sp ?extra_headers ~title ~div_id () = 
  unauth_template ~sp ?extra_headers ~title ~div_id () 
  >>= fun (ctxt, tmpl) ->
  begin
    match ctxt.role with 
      | User accnt | Admin accnt ->
          return (ctxt, tmpl, accnt) 
      | Anon ->
          fail PageRequiresAuth
  end

let init () =
  
  let () = 
    List.iter 
      (fun f -> ignore (f ()))
      [home; browse; upload; contribute; about]
  in

  let error_message s = 
    [p ~a:[a_class ["error"]] [pcdata s]]
  in

  let error_template ?code ~sp lst = 
    catch 
      (fun () -> 
         Account.box sp)
      (fun e ->
         return 
           (error_message
              (Printf.sprintf 
                 (f_ "Cannot get account box: %s")
                 (ODBMessage.string_of_exception e))))
    >>= fun account_box ->
    Eliom_predefmod.Xhtml.send ?code ~sp
      (template_skeleton
         ~sp 
         ~title:(OneTitle (s_ "Error"))
         ~div_id:"error_page"
         account_box
         lst)
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
             error_template ~sp 
               (error_message 
                  (s_ "Wrong parameters"))

         | PageRequiresAuth ->
             error_template ~sp 
               (error_message 
                  (s_ "You need to be logged in to see this page."))

         | Failure str ->
             error_template ~sp 
               (backtrace 
                  (error_message str))

         | e -> 
             error_template ~sp
               (backtrace 
                  (error_message
                     (Printexc.to_string e))))
