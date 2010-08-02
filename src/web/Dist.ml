
(** Access to ODBStorage dist directory
    @author Sylvain Le Gall
  *)

open Lwt
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Files
open ODBVer

let dist = 
  register_new_service 
    ~path:["dist"]
    ~get_params:(suffix (all_suffix "path"))
    (fun sp lst () ->
       let fn = 
         FilePath.make_filename 
           (ODBConf.dist_dir :: lst)
       in
         if FilePath.is_subdir ODBConf.dist_dir fn then
           (* TODO: log *)
           fail
             Eliom_common.Eliom_404
         else
           return 
             (print_endline ("Coucou "^fn);
              (FilePath.make_absolute (FileUtil.pwd ()) fn)))


let a_dist ~sp ver fcontent fn = 
 (* TODO: avoid too much ODBStorage lookup, by embeding
  * maximum information in ver (i.e. sexp.blackbox +
  * add information when loading
  *)
 ODBStorage.version_filename 
   ver.pkg 
   (OASISVersion.string_of_version ver.ver) 
   fn
 >>= fun fn ->
 let fn =
   let pwd =
     FileUtil.pwd ()
   in
     FilePath.make_relative 
       (FilePath.make_absolute pwd ODBConf.dist_dir)
       (FilePath.make_absolute pwd fn)
 in

 let lst = 
   (* TODO: create something in FilePath to handle 
    * this 
    *)
   ExtString.String.nsplit fn "/"
 in
   return 
     (Eliom_predefmod.Xhtml.a 
        (preapply dist lst)
        sp 
        (fcontent fn)
        (),
      fn)



