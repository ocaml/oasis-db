
open Lwt
open Eliom_parameters
open Eliom_sessions
open Eliom_services
open ODBVer
open OASISVersion
open CalendarLib
open ODBGettext
open Eliom_predefmod.Xhtml
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

let rss2 = 
  new_service
    ~path:["feeds"; "rss2"]
    ~get_params:unit
                      ()

let rss2_type = 
  "application/rss+xml"

let rss2_title =
  ns_ "OASIS-DB news"

let rss2_handler = 
  Eliom_predefmod.Text.register
    rss2
    (fun sp _ _ ->
       info () 
       >>= fun t ->
       begin
         try 
           let one_item ver =
             Rss.item
               ~title:(Printf.sprintf 
                         (f_ "Upload of %s v%s")
                         ver.pkg (string_of_version ver.ver))
               ~pubdate:(Rss.float_to_date 
                           (Calendar.to_unixfloat ver.upload_date)) 

               ~link:(make_string_uri 
                        ~sp 
                        ~absolute:true
                        ~service:(preapply browse (Some ver.pkg, Some ver.ver))
                        ())
               ()
           in
           let items =
             List.map one_item t.latest
           in
           let channel = 
             Rss.channel 
               ~title:(s_ rss2_title)
               ~link:(make_string_uri
                        ~sp
                        ~absolute:true
                        ~service:rss2
                        ())
               ~desc:(s_ "Uploads of OCaml packages on OASIS-DB and related news.")
               ~language:(s_ "en")
               items 
           in
           let content = 
             let buff = Buffer.create 13 in
             let fmt = Format.formatter_of_buffer buff in
               Rss.print_channel fmt channel; 
               Buffer.contents buff
           in
             return (content, rss2_type)
         with e ->
           fail e
       end)
