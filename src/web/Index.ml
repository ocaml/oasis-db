
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open ODBGettext
open ODBVer
open CalendarLib
open Template
open Common

type t = 
  {
    num_packages: int;
    num_uploads:  int;
    latest:       ODBVer.t list; 
    first_date:   Calendar.t;
    no_oasis:     int;
  }

let info () = 
  (* Get the n elements in front of the list *)
  let rec nhd n lst = 
    if n > 0 then 
      match lst with 
      | hd :: tl ->
          hd :: (nhd (n - 1) tl)
      | [] ->
          []
    else
      []
  in

  let nlatest = 
    20
  in

  ODBStorage.Pkg.elements () 
  >>= fun pkg_lst -> 
  Lwt_list.fold_left_s
    (fun t pkg ->
      ODBStorage.Ver.elements pkg
      >>= fun ver_lst ->
      ODBStorage.Ver.latest pkg
      >>= fun ver_latest ->
      ODBStorage.Ver.filename 
        pkg 
        (OASISVersion.string_of_version ver_latest.ver)
        `OASIS
      >>= fun oasis_fn ->

      (* Try to find a date older *)
      let first_date = 
        List.fold_left 
          (fun frst ver ->
            if Calendar.compare ver.upload_date frst < 0 then
              ver.upload_date
            else
              frst)
          t.first_date
          ver_lst
      in

      (* We are interested in the most recent
       * version of a package, in term of date
       * and among this among the most recent
       * in term of date for all packages
       *)
      let higher_versions = 
        nhd nlatest (List.rev ver_lst)
      in
      let latest = 
        nhd nlatest
          (List.sort 
            (fun v1 v2 ->
              Calendar.compare v2.upload_date v1.upload_date)
            (t.latest @ higher_versions))
      in

      (* Number of uploads = number of versions, no reason
       * to upload twice the same 
       *)
      let num_uploads = 
        t.num_uploads + List.length ver_lst
      in

      (* Count the number of missing oasis files 
       *)
      let no_oasis =
        if Sys.file_exists oasis_fn then
          t.no_oasis
        else
          t.no_oasis + 1 
      in

        return 
          {t with 
            num_uploads = num_uploads; 
            latest      = latest;
            first_date  = first_date;
            no_oasis    = no_oasis})

    {
      num_packages = List.length pkg_lst;
      num_uploads  = 0;
      latest       = [];
      first_date   = Calendar.now ();
      no_oasis     = 0;
    }
    pkg_lst

let home_handler sp () () =
   info () 
   >>= fun t ->
   Mkd.load "introduction" 
   >>= fun intro_html ->
   unauth_template 
     ~sp 
     ~title:(OneTitle (s_ "Home"))
     ~div_id:"home"
     ()
   >>= fun (_, tmpl) ->
   tmpl
     [
       div ~a:[a_class ["introduction"]] intro_html;

       div ~a:[a_class ["whatsnew"]]
         [
           h2 [pcdata "What's new"];

           (match t.latest with
              | hd :: tl ->
                  begin
                    let to_li ver =
                      let pkg = 
                        ver.pkg 
                      in
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
                                   pkg ver_s date)]
                             (Some pkg, Some ver_s)]
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
  Mkd.load "contribute"
  >>= fun contribute_html ->
  unauth_template 
    ~sp 
    ~title:(OneTitle (s_ "Contribute"))
    ~div_id:"contribute"
    ()
  >>= fun (_, tmpl) ->
  tmpl contribute_html

let about_handler sp () () =
  Mkd.load "about"
  >>= fun about_html ->
  unauth_template 
    ~sp 
    ~title:(OneTitle (s_ "About this website"))
    ~div_id:"about"
    () 
  >>= fun (_, tmpl) ->
  tmpl about_html

let init server = 
  ()
