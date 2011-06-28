
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
open Context

type t = 
  {
    num_packages: int;
    num_uploads:  int;
    latest:       ODBLog.t list; 
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

  let sqle = ctxt.sqle in
  let stor = ctxt.stor in

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
        List.rev
          (sort_log 
             (SetLog.elements (combine SetLog.empty events)))
      in
        Lwt_list.fold_left_s
          (fun (count, acc) log ->
             if count <= 0 then
               return (count, acc)
             else
               catch
                 (fun () ->
                    match log.log_event with 
                      | `Pkg (pkg_str, `VersionCreated ver) ->
                          begin
                            ODBStorage.PkgVer.mem stor (`StrVer (pkg_str, ver))
                            >|= fun exists ->
                              if exists then
                                count - 1, log :: acc
                              else
                                count, acc
                          end
                      | _ ->
                          return (count, acc))
                 (fun _ ->
                    return (count, acc)))
          (nlatest, [])
          log_latest
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

let make_feed ~ctxt ~sp title desc service ?string_of_log lst =  
  Lwt_list.map_s 
    (fun log ->
       let failsafe_oasis_latest pkg_str =
         catch 
           (fun () -> 
              ODBStorage.PkgVer.latest ctxt.stor (`Str pkg_str)
              >>= fun pkg_ver ->
              ODBStorage.PkgVer.oasis ctxt.stor (`PkgVer pkg_ver))
           (fun e ->
              return None)
       in
       let failsafe_oasis pkg_str k = 
         catch 
           (fun () ->
              ODBStorage.PkgVer.oasis ctxt.stor k)
           (fun e ->
              failsafe_oasis_latest pkg_str)
       in
         begin
           match log.log_event with 
             | `Pkg (pkg_str, `VersionCreated ver) 
             | `Pkg (pkg_str, `VersionDeleted ver) ->
                 failsafe_oasis pkg_str (`StrVer (pkg_str, ver))
             | `Pkg (pkg_str, _) ->
                 failsafe_oasis_latest pkg_str
             | `Sys _ ->
                 return None
         end
         >>= fun oasis_opt ->
         begin
           match log.log_event with 
             | `Pkg (pkg_str, `VersionCreated ver) ->
                 begin
                   ODBStorage.PkgVer.mem ctxt.stor (`StrVer (pkg_str, ver))
                   >>= fun exists ->
                   begin
                     if exists then 
                       return
                         (Some 
                            (preapply view_pkg_ver 
                               (pkg_str, 
                                Version ver)))
                     else
                       return None
                   end
                 end
              | `Pkg (pkg_str, _) ->
                  begin
                    catch 
                      (fun () ->
                         ODBStorage.PkgVer.latest ctxt.stor (`Str pkg_str)
                         >>= fun pkg_ver ->
                         return
                           (Some 
                              (preapply view_pkg_ver
                                 (pkg_ver.pkg,
                                  Version pkg_ver.ver))))
                      (fun _ ->
                         ODBStorage.Pkg.mem ctxt.stor (`Str pkg_str)
                         >>= fun exists ->
                         begin
                           if exists then 
                             return 
                               (Some 
                                  (preapply view_pkg_ver
                                     (pkg_str, NoVersion)))
                           else
                             return 
                               None
                         end)
                  end

             | `Sys _ ->
                 return None
         end
         >|= fun srvc_opt ->
         log, oasis_opt, srvc_opt)
    lst
  >>= fun lst ->
  try
    let lst = 
      List.sort
        (fun (log1, _, _) (log2, _, _) ->
           Calendar.compare
             log2.log_timestamp
             log1.log_timestamp)
        lst
    in

    let one_item (log, oasis_opt, srvc_opt) =
      let desc = 
        match oasis_opt with 
          | Some oasis ->
              let desc = 
                (h3 [pcdata oasis.OASISTypes.synopsis])
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
      let title = 
        match string_of_log with 
          | Some f ->
              f log
          | None ->
              ODBLog.to_string log
      in
      let link = 
        match srvc_opt with 
          | Some service ->
              Some 
                (string_of_uri
                   (OCAWeb.Redirect.rewrite ~ctxt:ctxt.ocaw sp
                      (make_uri ~sp ~absolute:true ~service ())))
          | None ->
              None
      in
        Rss.item ~title ?link ?desc
          ~pubdate:(Rss.float_to_date 
                      (Calendar.to_unixfloat log.log_timestamp)) 
          ()
    in
    let items =
      List.map one_item lst
    in
    let channel = 
      Rss.channel 
        ~title
        ~link:(string_of_uri
                 (OCAWeb.Redirect.rewrite
                    ~ctxt:ctxt.ocaw
                    sp 
                    (make_uri ~sp ~absolute:true ~service ())))
        ~desc
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

let () = 
  Eliom_predefmod.Text.register
    rss2
    (fun sp _ _ ->
       Context.get ~sp ()
       >>= fun ctxt ->
       info ~ctxt () 
       >>= fun t ->
       make_feed ~ctxt ~sp 
         (s_ rss2_title)
         (s_ "Uploads of OCaml packages on OASIS-DB and related news.")
         rss2
         ~string_of_log:
         (function
            | {log_event = `Pkg (pkg_str, `VersionCreated ver)} ->
                Printf.sprintf 
                  (f_ "Upload of %s v%s")
                  pkg_str (string_of_version ver)
            | log ->
                ODBLog.to_string log)
         t.latest)
