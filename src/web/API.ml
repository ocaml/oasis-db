
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
  RESTOcsigen.register_new_service base_path api_fun

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
    ~path:(base_path @ ["help"])
    ~get_params:unit
    (fun sp path () ->
       Context.get ~sp () 
       >|= fun ctxt ->
       template 
         ~ctxt 
         ~sp
         ~title:(OneTitle (s_ "API"))
         ~div_id:"api"
         (RESTOcsigen.help_box ~sp  MarkdownExt.to_html ()))

let login = 
  Eliom_predefmod.Redirection.register_new_service
    ~path:(base_path @ ["login"])
    ~get_params:(string "login" ** string "password")
    (fun sp (login, password) () ->
       return (preapply Account.login_get_ext (login, password)))

let logout = 
  Eliom_predefmod.Redirection.register_new_service
    ~path:(base_path @ ["logout"])
    ~get_params:unit
    (fun sp () () ->
       return Account.logout_get_ext)

let pkg_ver_upload_no_post = 
  Eliom_predefmod.Unit.register_new_service
    ~path:(base_path @ ["upload"])
    ~get_params:unit
    (fun sp path () ->
       Context.get ~sp () 
       >>= fun ctxt ->
       fail (Failure (s_ "This page require POST params")))

let pkg_ver_upload =
  Eliom_predefmod.Unit.register_new_post_service 
    ~fallback:pkg_ver_upload_no_post
    ~post_params:(string "publink" ** file "tarball")
    (fun sp _ (publink, tarball_fd) ->
       Context.get_user ~sp ()
       >>= fun (ctxt, accnt) ->
       Upload.upload_init_check tarball_fd publink
       >>= fun (tarball_fn, tarball_nm, publink) ->
       ODBUpload.upload_begin ~ctxt:ctxt.odb
         (WebAPI accnt.Account.accnt_name)
         tarball_fn tarball_nm publink 
       >>= fun upload ->
       ODBUpload.upload_commit ~ctxt:ctxt.odb
         upload
       >|= fun _ ->
       ())