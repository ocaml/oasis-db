
open ODBGettext
open OASISVersion
open Lwt

(** Current version of the API *)
let current_version =
  chop (version_of_string ODBConf.version)

(** Former version of the API *)
let past_versions = 
  List.rev_map
    version_of_string 
    [] (* 0.1, 0.2, 0.3... *)

module ODBAPI = 
  REST.Make 
    (struct 
       let current_version = current_version
       let past_versions   = past_versions
     end)

open ODBAPI
open REST

let mk ~ctxt api_fun base_url params = 
  let curl_socket = 
    Curl.init ()
  in
  let ctxt = 
    RESTCurl.create 
      ~dbug_print:(fun s -> Lwt_main.run (ODBMessage.debug ~ctxt "%s" s))
      base_url curl_socket
  in
  let res =
    RESTCurl.mk_fun api_fun ~ctxt params
  in
    Curl.cleanup curl_socket;
    res

module Pkg = 
struct 
  let section =
    {
      sct_title = ns_ "Package management";
      sct_help  = ns_ "Manage packages.";
      sct_order = 0;
      sct_base  = ["pkg"];
    }

  let def_list = 
    create
      ~section
      ~title:(ns_ "List packages")
      ~path:["list"]
      ~params:RESTParams.unit
      ~valid_versions:">= 0.1"
      ~help:
      "This function list packages."
      ~examples:
      [
        "",
        (),
        ["oasis"; "ocaml-data-notation"; "ocamlify"]
      ]
      ODBStorage.Pkg.elements 
      (RESTConv.list RESTConv.string)

  let list = 
    mk def_list

end

module PkgVer =
struct 
  open RESTParams
  open RESTConv
  open ODBPkgVer

  let section =
    {
      sct_title = ns_ "Package's version management";
      sct_help  = ns_ "Manage package's version.";
      sct_order = 10;
      sct_base  = ["pkg_ver"];
    }


  let conv_t = 
    {
      to_sexp = ODBPkgVer.sexp_of_t;
      of_sexp = ODBPkgVer.t_of_sexp;

      to_json = 
        (fun t -> 
           `Assoc 
             ["pkg",    `String t.pkg;
              "ver",    `String (string_of_version t.ver);
              "ord",    `Int t.ord;
              "tarball",`String t.tarball;
              "upload_date", 
              `String 
                (CalendarLib.Printer.Calendar.to_string 
                   t.upload_date);

              "upload_method", 
              `Assoc
                (match t.upload_method with
                   | Web nm -> 
                       ["type", `String "Web";
                        "name", `String nm]

                   | WebAPI nm -> 
                       ["type", `String "Manual";
                        "name", `String nm]

                   | Uscan -> 
                       ["type", `String "Uscan"]

                   | OCamlForge -> 
                       ["type", `String "OCamlForge"]

                   | Incoming nm ->
                       ["type", `String "Incoming";
                        "name", `String nm]
                );

              "publink", 
              (match t.publink with 
                 | Some s -> `String s
                 | None -> `Null)
             ]);
      of_json = (fun _ -> failwith "TODO");
    }

  let def_list = 
    create
      ~path:["list"]
      ~valid_versions:">= 0.1"
      ~params:(RESTParams.string "pkg")
      ~section
      ~title:(ns_ "List versions of a package.")
      ~help:
      "This function list package's versions."
      ~examples:
      [
        "",
        "oasis",
        List.map version_of_string ["0.1.0"; "0.2.0"]
      ]
      (fun pkg ->
         ODBStorage.PkgVer.elements pkg
         >|=
         List.map (fun ver -> ver.ODBPkgVer.ver))
      (list version)

  let list = 
    mk def_list

  let def_latest = 
    create
      ~path:["latest"]
      ~valid_versions:">= 0.1"
      ~params:(RESTParams.string "pkg")
      ~section
      ~title:(ns_ "Get the latest version of a package.")
      ~help:"Return the latest version available. The latest version is defined by
             order and not by the version number."
      ~examples:
      [
        "",
        "oasis",
        OASISVersion.version_of_string "0.2.0"
      ]
      (fun pkg ->
         (ODBStorage.PkgVer.latest pkg)
         >|= fun ver ->
         ver.ODBPkgVer.ver)
      version

  let latest = 
    mk def_latest

  let def_show =
    create
      ~path:["show"]
      ~valid_versions:">= 0.1"
      ~params:(RESTParams.string "pkg" ** RESTParams.string "ver")
      ~section
      ~title:(ns_ "Display package's version information")
      ~help: 
      "Display information about a package, as defined during the upload
       step. You could also extract information from the _oasis file."
      ~examples:
      [
        "",
        ("oasis", "0.2.0"),
        {
          pkg = "oasis";
          ver = (OASISVersion.version_of_string "0.2.0");
          ord = 10;
          tarball = "oasis-0.2.0.tar.gz";
          upload_date = CalendarLib.Calendar.now ();
          upload_method = Web "gildor";
          publink = 
            Some 
              "https://forge.ocamlcore.org/frs/download.php/501/oasis-0.2.0.tar.gz";
        }
      ]
      (fun (pkg, ver) ->
         ODBStorage.PkgVer.find pkg ver)
      conv_t

  let show = 
    let f = mk def_show in
      (fun ~ctxt base_url pkg ver -> 
         f base_url (pkg, ver))

end

