
open Eliom_services
open Eliom_parameters
open Template
open Lwt
open ODBREST
open ODBGettext
open Context
open ODBPkgVer

let base_path = 
  ["api"]

let register_new_service api_fun = 
  RESTOcsigen.register_new_service 
    (fun sp ->
       Context.get ~sp ()
       >>= fun ctxt ->
       return {rst_stor = ODBStorage.to_ro ctxt.stor})
    base_path api_fun

let pkg_list = 
  register_new_service Pkg.def_list

let pkg_ver_list = 
  register_new_service PkgVer.def_list

let pkg_ver_latest = 
  register_new_service PkgVer.def_latest

let pkg_ver_show =
  register_new_service PkgVer.def_show

let api_help = 
  Eliom_predefmod.Xhtml.register_new_service
    ~path:(base_path @ [""])
    ~get_params:unit
    (fun sp path () ->
       Context.get ~sp () 
       >>= fun ctxt ->
       template 
         ~ctxt 
         ~sp
         ~title:(OneTitle (s_ "API"))
         ~div_id:"api"
         (RESTOcsigen.help_box ~sp  MarkdownExt.to_html ()))

let login = 
  Eliom_predefmod.String_redirection.register_new_service
    ~path:(base_path @ ["login"])
    ~get_params:(string "login" ** string "password")
    (fun sp (login, password) () ->
       Context.get ~sp () 
       >>= fun ctxt ->
       return (Session.login_get_ext ~ctxt sp login password))

let logout = 
  Eliom_predefmod.String_redirection.register_new_service
    ~path:(base_path @ ["logout"])
    ~get_params:unit
    (fun sp () () ->
       Context.get ~sp () 
       >>= fun ctxt ->
       return (Session.logout_get_ext ~ctxt sp))

let pkg_ver_upload_no_post = 
  Eliom_predefmod.Unit.register_new_service
    ~path:(base_path @ ["upload"])
    ~get_params:unit
    (fun sp path () ->
       fail (Failure (s_ "This page require POST params")))

let pkg_ver_upload =
  Eliom_predefmod.Text.register_new_post_service 
    ~fallback:pkg_ver_upload_no_post
    ~post_params:(opt (string "publink") ** file "tarball")
    (fun sp _ (publink_opt, tarball_fd) ->
       Context.get_user ~sp ()
       >>= fun (ctxt, accnt) ->
       Upload.upload_init_check tarball_fd publink_opt
       >>= fun (tarball_fn, tarball_nm, publink_opt) ->
       ODBUpload.upload_begin ~ctxt:ctxt.odb
         ctxt.stor
         (WebAPI accnt.OCAAccount.accnt_real_name)
         tarball_fn tarball_nm publink_opt 
       >>= fun upload ->
       ODBUpload.upload_commit ~ctxt:ctxt.odb
         upload
       >|= fun (_, pkg_ver) ->
       Printf.sprintf
         (f_ "Package's version %s v%s successfully uploaded.")
         pkg_ver.pkg (OASISVersion.string_of_version pkg_ver.ver),
       "text/plain")
