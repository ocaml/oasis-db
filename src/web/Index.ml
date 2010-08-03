
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

let _ = 
  ODBMain.run ~ctxt:ODBContext.default ()

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

  ODBStorage.packages () 
  >>= fun pkg_lst -> 
  Lwt_list.fold_left_s
    (fun t pkg ->
      ODBStorage.versions pkg
      >>= fun ver_lst ->
      ODBStorage.version_latest pkg
      >>= fun ver_latest ->
      ODBStorage.version_filename 
        pkg 
        (OASISVersion.string_of_version ver_latest.ver)
        ODBStorage.OASIS
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

let _ = 
  register
    home
    (fun sp () () ->
      info () 
      >>= fun t ->
      page_template sp (s_ "Home") Account.box
        [
          div ~a:[a_id "home_highlight"]
            [
              ul
                (li [a browse sp [pcdata (s_ "Browse packages")] ()])
                [li [a upload sp [pcdata (s_ "Upload your package")] ()]];
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
                                (preapply 
                                  Browse.browse_pkg_ver 
                                  (pkg, ver_s))
                                sp
                                [pcdata 
                                   (Printf.sprintf 
                                      (f_ "%s v%s (%s)")
                                      pkg ver_s date)]
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
              p 
                [pcdata 
                  (Printf.sprintf 
                    (f_ "%d uploads and %d packages in the database since %s")
                    t.num_uploads
                    t.num_packages
                    (Printer.Calendar.sprint "%F" t.first_date))];

              p
                [pcdata
                   (Printf.sprintf 
                      (f_ "%d%% of the latest package's versions have an _oasis file.")
                      (((t.num_packages - t.no_oasis) * 100) / t.num_packages))]
            ];
        ])


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
      (*ignore (tic_tac ());*)
      page_template sp (s_ "About this website") Account.box
        [p [pcdata "About"]])
