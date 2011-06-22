
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
open XHTML.M

type t = 
  {
    num_packages: int;
    num_uploads:  int;
    latest:       ODBPkgVer.t list; 
    first_date:   Calendar.t;
    no_oasis:     int;
  }

module SetLog = 
  Set.Make
    (struct
       type t = ODBLog.t
       let compare t1 t2 = 
         Pervasives.compare 
           t1.log_event t2.log_event
     end)


let info ~ctxt () = 
  let nlatest = 
    20
  in

  let sqle = ctxt.Context.sqle in
  let stor = ctxt.Context.stor in

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
    Log.get ~filter:(`Event `VersionCreated) sqle 
    >>= fun log_add_latest ->
    Log.get ~filter:(`Event `VersionDeleted) sqle
    >>= fun log_rm_latest -> 
    begin 
      let sort_log = 
        List.sort
          (fun log1 log2 -> 
             Calendar.compare 
               log1.log_timestamp
               log2.log_timestamp)
      in
      let events = 
        sort_log (List.rev_append log_add_latest log_rm_latest)
      in
      let rec combine st =
        function
          | {log_event = `Pkg (_, `VersionCreated _)} as log :: tl ->
              combine (SetLog.add log st) tl
          | {log_event = `Pkg (_, `VersionDeleted _)} as log :: tl ->
              combine (SetLog.remove log st) tl 
          | _ :: tl ->
              combine st tl
          | [] ->
              st
      in
      let log_latest = 
        sort_log 
          (SetLog.elements (combine SetLog.empty events))
      in
      let latest = 
        List.fold_left 
          (fun acc ->
             function
               | {log_event = `Pkg (pkg_str, `VersionCreated ver)} ->
                   (pkg_str, OASISVersion.string_of_version ver) :: acc
               | _ ->
                   acc)
          []
          log_latest
      in
        Lwt_list.fold_left_s
          (fun (count, acc) (pkg_str, ver_str) ->
             if count <= 0 then
               return (count, acc)
             else
               catch 
                 (fun () ->
                    ODBStorage.PkgVer.find stor (`Str (pkg_str, ver_str))
                    >>= fun pkg_ver ->
                    return (count - 1, pkg_ver :: acc))
                 (function 
                    | Not_found ->
                        return (count, acc)
                    | e ->
                        fail e))
          (nlatest, [])
          latest
        >|= fun (_, lst) ->
        List.rev lst
    end
  end
  >>= fun latest ->

  ODBStorage.Pkg.elements stor
  >>= fun pkg_lst -> 
  begin
    Lwt_list.fold_left_s
      (fun acc pkg ->
         catch
           (fun () ->
              ODBStorage.PkgVer.latest stor (`Pkg pkg)
              >|= fun ver_latest ->
              ver_latest :: acc)
           (function
              | Not_found ->
                  return acc
              | e ->
                  fail e))
      []
      pkg_lst
      >>=
      Lwt_list.filter_p
        (fun pkg_ver_latest ->
          ODBStorage.PkgVer.oasis_status stor (`PkgVer pkg_ver_latest)
          >|= 
            function
              | `OK -> false
              | `Not_found | `Error -> true)
  end

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
       Lwt_list.map_s 
         (fun pkg_ver ->
            ODBStorage.PkgVer.oasis ctxt.Context.stor (`PkgVer pkg_ver)
            >|= fun oasis ->
            pkg_ver, oasis)
         t.latest
       >>= fun pkg_ver_oasis_lst ->
       begin
         try 
           let one_item (pkg_ver, oasis_opt) =
             let desc = 
               match oasis_opt with 
                 | Some oasis ->
                     let desc = 
                       (h1 [pcdata oasis.OASISTypes.synopsis])
                       ::
                       (match oasis.OASISTypes.description with 
                          | Some txt -> 
                              (MarkdownExt.to_html txt)
                          | None -> 
                              [])
                     in
                       Some (Xhtmlcompact.xhtml_list_print desc)
                 | None ->
                     None
             in
               Rss.item
                 ~title:(Printf.sprintf 
                           (f_ "Upload of %s v%s")
                           pkg_ver.pkg (string_of_version pkg_ver.ver))
                 ~pubdate:(Rss.float_to_date 
                             (Calendar.to_unixfloat pkg_ver.upload_date)) 

                 ~link:(string_of_uri
                          (OCAWeb.Redirect.rewrite 
                             ~ctxt:ctxt.Context.ocaw
                             sp
                             (make_uri 
                                ~sp 
                                ~absolute:true
                                ~service:(preapply view_pkg_ver (pkg_ver.pkg, Version pkg_ver.ver))
                                ())))
                 ?desc
                 ()
           in
           let items =
             List.map one_item pkg_ver_oasis_lst
           in
           let channel = 
             Rss.channel 
               ~title:(s_ rss2_title)
               ~link:(string_of_uri
                        (OCAWeb.Redirect.rewrite
                           ~ctxt:ctxt.Context.ocaw
                           sp 
                           (make_uri
                              ~sp
                              ~absolute:true
                              ~service:rss2
                              ())))
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
