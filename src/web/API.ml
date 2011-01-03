
open Eliom_services
open Eliom_parameters
open Template
open Lwt
open ODBREST
open ODBGettext

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
