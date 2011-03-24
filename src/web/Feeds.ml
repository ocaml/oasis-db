
open Lwt
open Eliom_parameters
open Eliom_sessions
open Eliom_services
open ODBLog
open ODBPkgVer
open OASISVersion
open CalendarLib
open ODBGettext
open Eliom_predefmod.Xhtml
open Common

type t = 
  {
    num_packages: int;
    num_uploads:  int;
    latest:       ODBPkgVer.t list; 
    first_date:   Calendar.t;
    no_oasis:     int;
  }

let info ~ctxt () = 
  let nlatest = 
    20
  in

  let sqle = 
    ctxt.Context.sqle
  in

  begin
    Log.get_rev ~filter:(`Event `VersionCreated) ~limit:1 sqle
    >|= 
      function 
        | first :: _ ->
            first.log_timestamp
        | [] ->
            CalendarLib.Calendar.now ()
  end
  >>= fun first_date ->

  Log.get_count ~filter:(`Event `VersionCreated) sqle
  >>= fun num_uploads ->

  begin
    Log.get ~filter:(`Event `VersionCreated) ~limit:nlatest sqle
    >>= fun log_latest ->
    let latest = 
      (List.fold_left 
         (fun acc ->
            function
              | {log_event = `Pkg (pkg_str, `VersionCreated ver)} ->
                  (pkg_str, OASISVersion.string_of_version ver) :: acc
              | _ ->
                  acc)
         []
         log_latest)
    in
      Lwt_list.fold_left_s
        (fun acc (pkg_str, ver_str) ->
           catch 
             (fun () ->
                ODBStorage.PkgVer.find pkg_str ver_str
                >>= fun pkg_ver ->
                return (pkg_ver :: acc))
             (function 
                | Not_found ->
                    ODBMessage.warning ~ctxt:ctxt.Context.odb 
                      (f_ "Latest upload of package's version %s v%s not found")
                      pkg_str ver_str
                    >>= fun () ->
                    return acc
                | e ->
                    fail e))
        []
        latest
  end
  >>= fun latest ->

  ODBStorage.Pkg.elements () 
  >>= fun pkg_lst -> 
  Lwt_list.filter_p
    (fun {ODBPkg.pkg_name = pkg_str} ->
      ODBStorage.PkgVer.latest pkg_str
      >>= fun ver_latest ->
      ODBStorage.PkgVer.filename 
        pkg_str
        (OASISVersion.string_of_version ver_latest.ver)
        `OASIS
      >>= fun oasis_fn ->
      return (not (Sys.file_exists oasis_fn)))
    pkg_lst
  >|= fun pkg_without_oasis_lst ->

  {
    num_packages = List.length pkg_lst;
    num_uploads  = num_uploads;
    latest       = latest;
    first_date   = first_date;
    no_oasis     = List.length pkg_without_oasis_lst;
  }

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
       Context.get ~sp ()
       >>= fun ctxt ->
       info ~ctxt () 
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
                        ~service:(preapply view (ver.pkg, Version ver.ver))
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
