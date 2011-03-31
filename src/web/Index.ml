
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

let upload_chart ~ctxt () = 
  begin
    let fst_date = 
      let month_ago =
        Calendar.rem
          (Calendar.now ()) 
          (Calendar.Period.month 11)
      in
        Calendar.lmake
          ~year:(Calendar.year month_ago)
          ~month:(Date.int_of_month (Calendar.month month_ago))
          ()
    in
      Log.upload_stats ctxt.Context.sqle fst_date
  end
  >|= fun lst ->
  begin
    let catmap ?(sep=",") f = 
      String.concat sep (List.map f lst)
    in
    let axis_month = 
      catmap ~sep:"|"
        (fun (date, _, _) -> 
           Printer.Calendar.sprint "%b" date)
    in
    let axis_year =
      let lst =
        List.map
          (fun (date, _, _) ->
             Printer.Calendar.sprint "%Y" date)
          lst
      in
      let rec dedup prev_opt lst = 
        match prev_opt, lst with
          | None, hd :: tl ->
              hd :: (dedup (Some hd) tl)
          | Some y, hd :: tl ->
              if y = hd then
                "" :: (dedup prev_opt tl)
              else
                hd :: (dedup (Some hd) tl)
          | _, [] ->
              []
      in
        String.concat "|" (dedup None lst)
    in
    let num_pkg = 
      catmap 
        (fun (_, num_pkg, _) ->
           string_of_int num_pkg)
    in
    let uploads =
      catmap
        (fun (_, _, uploads) ->
           string_of_int uploads)
    in
      img
        ~alt:"Package uploads graph"
        ~src:(uri_of_string
                (Printf.sprintf
                   "http://chart.googleapis.com/chart?\
                      chxl=0:|%s|1:|%s|2:|10|20|30|50|100|&chxt=x,x,y&\
                      cht=bvg&chs=401x187&\
                      chd=t1:%s|%s&\
                      chm=D,000000,1,0,3,1&\
                      chco=E6E6E6&\
                      chf=bg,s,EEEEEE00"
                 axis_month axis_year
                 uploads num_pkg))
        ()
  end
        

    (* TODO: activate when offline 
          img 
            ~alt:"Package uploads graph"
            ~src:(mk_static_uri sp ["chart-upload.png"])
            ();
     *)

let home_handler sp () () =
  Context.get ~sp () 
  >>= fun ctxt ->
  info ~ctxt () 
  >>= fun t ->
  Mkd.load ctxt.Context.mkd "introduction" 
  >>= fun intro_html ->
  upload_chart ~ctxt ()
  >>= fun upload_chart_box ->
  template 
    ~ctxt 
    ~sp 
    ~title:NoTitle
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
                            view
                            sp
                            [pcdata 
                               (Printf.sprintf 
                                  (f_ "%s v%s (%s)")
                                  ver.pkg ver_s date)]
                            (ver.pkg, Version ver.ver)]
                   in
                     ul (to_li hd) (List.map to_li tl)
                 end
             | [] ->
                 pcdata "");
        ];

      div ~a:[a_class ["statistics"]]
        [
          h2 [pcdata "Statistics"];
          upload_chart_box;

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
  Mkd.load ctxt.Context.mkd "contribute"
  >>= 
  template 
    ~ctxt
    ~sp 
    ~title:(OneTitle (s_ "Contribute"))
    ~div_id:"contribute"

let about_handler sp () () =
  Context.get ~sp () 
  >>= fun ctxt ->
  Mkd.load  ctxt.Context.mkd "about"
  >>=
  template 
    ~ctxt
    ~sp 
    ~title:(OneTitle (s_ "About this website"))
    ~div_id:"about"
