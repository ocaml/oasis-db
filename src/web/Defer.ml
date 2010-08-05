
(** Defer registration of services 
    @author Sylvain Le Gall
  *)

open Eliom_services

let mk f =
  let lz =
    Lazy.lazy_from_fun f
  in
    fun () -> Lazy.force lz

let new_service path params = 
  mk
    (fun () -> new_service path params ())

let register_new_service ~path ~get_params f = 
  mk
    (fun () -> 
       Eliom_predefmod.Xhtml.register_new_service ~path ~get_params f)

let register service f = 
  mk
    (fun () -> Eliom_predefmod.Xhtml.register (service ()) f)

module Files =
struct 
  let register_new_service ~path ~get_params f =
    mk
      (fun () -> 
         Eliom_predefmod.Files.register_new_service 
           ~path ~get_params f)
end

module Redirection =
struct
  let register_new_post_service ~post_params ~fallback f = 
    mk
      (fun () ->
         Eliom_predefmod.Redirection.register_new_post_service 
           ~post_params ~fallback:(fallback ()) f)

  let register_new_service ~path ~get_params f = 
    mk 
      (fun () ->
         Eliom_predefmod.Redirection.register_new_service
           ~path ~get_params f)
end
