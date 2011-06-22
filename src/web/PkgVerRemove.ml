
(** Package's version removal 
  * @author Sylvain Le Gall
  *)

open Lwt
open XHTML.M
open Eliom_parameters
open Eliom_predefmod.Xhtml
open Eliom_services
open Template
open ODBPkgVer
open ODBGettext
open OASISVersion
open Common
open Context

let remove = 
  register_new_service 
    ~path:["remove_pkg_ver"]
    ~get_params:(string "pkg" **
                 ExtParams.version "ver")
    (fun sp (pkg_str, ver) () ->
       Context.get_user ~sp () 
       >>= fun (ctxt, _) ->
       begin
         let confirm = 
           Eliom_predefmod.Redirection.register_new_coservice_for_session'
             ~sp
             ~get_params:(string "action" **
                          string "pkg" **
                          ExtParams.version "ver")
             (fun sp (action, (pkg_str, ver)) () ->
                Context.get_user ~sp () 
                >>= fun (ctxt, _) ->
                  if action = "confirm" then
                    begin
                      ODBStorage.PkgVer.remove 
                        ctxt.stor 
                        (`StrVer (pkg_str, ver))
                      >>= fun () ->
                      return home
                    end
                  else
                    begin
                      return 
                        (preapply view_pkg_ver (pkg_str, Version ver))
                    end)
         in
           template ~ctxt ~sp
             ~title:(OneTitle 
                       (Printf.sprintf 
                          (f_ "Remove %s v%s")
                          pkg_str
                          (string_of_version ver)))
             ~div_id:"remove"
              [
                pcdata 
                  (Printf.sprintf 
                     (f_ "You are about to remove %s v%s.")
                     pkg_str
                     (string_of_version ver));
                br ();
                br ();
                em
                  [pcdata (s_ "Are you sure you want to proceed!")];

                get_form
                  ~service:confirm
                  ~sp
                  (fun (action_nm, (pkg_nm, ver_nm)) ->
                     [p 
                        [string_button
                           ~name:action_nm
                           ~value:"cancel"
                           [pcdata (s_ "Cancel")];
                         string_button
                           ~name:action_nm
                           ~value:"confirm"
                           [pcdata (s_ "Remove")];
                         string_input
                           ~name:pkg_nm
                           ~input_type:`Hidden
                           ~value:pkg_str
                           ();
                         user_type_input
                           ~name:ver_nm
                           ~input_type:`Hidden
                           ~value:ver
                           string_of_version
                           ();
                        ]
                     ])
              ]
       end)

let box ~sp ~ctxt pkg_ver = 
  return 
    (Account.link_need_login ~sp ~ctxt
       (s_ "Remove")
       (preapply remove (pkg_ver.pkg, pkg_ver.ver)))
