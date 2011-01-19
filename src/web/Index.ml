
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open ODBGettext
open ODBPkgVer
open CalendarLib
open Template
open Common
open Feeds

let home_handler sp () () =
  Context.get ~sp () 
  >>= fun ctxt ->
  info () 
  >>= fun t ->
  Mkd.load "introduction" 
  >|= fun intro_html ->
  template 
    ~ctxt 
    ~sp 
    ~title:(OneTitle (s_ "Home"))
    ~div_id:"home"
    ~extra_headers:[link 
                      ~a:[a_rel [`Alternate];
                          a_href (make_uri 
                                    ~sp
                                    ~service:rss2
                                    ());
                          a_type rss2_type;
                          a_title (s_ rss2_title)]
                      ()]
    [
      div ~a:[a_class ["introduction"]] intro_html;

      div ~a:[a_class ["whatsnew"]]
        [
          h2 [pcdata "What's new"];

          (match t.latest with
             | hd :: tl ->
                 begin
                   let to_li ver =
                     let date =
                       CalendarLib.Printer.Calendar.to_string 
                         ver.upload_date
                     in
                     let ver_s = 
                       OASISVersion.string_of_version 
                         ver.ver
                     in
                       li 
                         [a 
                            browse
                            sp
                            [pcdata 
                               (Printf.sprintf 
                                  (f_ "%s v%s (%s)")
                                  ver.pkg ver_s date)]
                            (Some ver.pkg, Some ver.ver)]
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
          p 
            [pcdata 
              (Printf.sprintf 
                (f_ "%d uploads and %d packages in the database since %s")
                t.num_uploads
                t.num_packages
                (Printer.Calendar.sprint "%F" t.first_date))];
          
          p
            [pcdata
               begin
                 if t.num_packages > 0 then
                   (Printf.sprintf 
                      (f_ "%d%% of the latest package's versions \
                           have an _oasis file.")
                      (((t.num_packages - t.no_oasis) * 100) 
                      / t.num_packages))
                 else
                   s_ "No packages"
               end
            ]
        ];            
    ]


let contribute_handler sp () () =
  Context.get ~sp () 
  >>= fun ctxt ->
  Mkd.load "contribute"
  >|= 
  template 
    ~ctxt
    ~sp 
    ~title:(OneTitle (s_ "Contribute"))
    ~div_id:"contribute"

let about_handler sp () () =
  Context.get ~sp () 
  >>= fun ctxt ->
  Mkd.load "about"
  >|=
  template 
    ~ctxt
    ~sp 
    ~title:(OneTitle (s_ "About this website"))
    ~div_id:"about"
