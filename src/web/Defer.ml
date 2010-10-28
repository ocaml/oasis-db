
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

let new_post_coservice ~post_params ~fallback f = 
  mk
    (fun () ->
       new_post_coservice
         ~post_params 
         ~fallback:(fallback ())
         f)

let register_new_service ~path ~get_params f = 
  mk
    (fun () -> 
       Eliom_predefmod.Xhtml.register_new_service ~path ~get_params f)

let register_new_post_service ~post_params ~fallback f = 
  mk
    (fun () ->
       Eliom_predefmod.Xhtml.register_new_post_service 
         ~post_params 
         ~fallback:(fallback ()) 
         f)

let register_new_post_coservice ~post_params ~fallback f = 
  mk
    (fun () ->
       Eliom_predefmod.Xhtml.register_new_post_coservice 
         ~post_params 
         ~fallback:(fallback ()) 
         f)

let register service f = 
  mk
    (fun () -> Eliom_predefmod.Xhtml.register (service ()) f)

let new_external_service ~prefix ~path ~get_params ~post_params () =
  mk 
    (fun () ->
       Eliom_services.new_external_service 
         ~prefix ~path ~get_params ~post_params ())

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

module Any =
struct
  let register_new_service ~path ~get_params f = 
    mk 
      (fun () ->
         Eliom_predefmod.Any.register_new_service
           ~path ~get_params f)

  let register_new_post_service ~post_params ~fallback f = 
    mk
      (fun () ->
         Eliom_predefmod.Any.register_new_post_service 
           ~post_params ~fallback:(fallback ()) f)
end

module Action =
struct 
  let register_new_post_coservice ~post_params ~fallback f = 
    mk
      (fun () ->
         Eliom_predefmod.Action.register_new_post_coservice
           ~post_params 
           ~fallback:(fallback ())
           f)

  let register_new_post_coservice' ~post_params f = 
    mk
      (fun () ->
         Eliom_predefmod.Action.register_new_post_coservice'
           ~post_params 
           f)

  let register service f =  
    mk
      (fun () -> Eliom_predefmod.Action.register (service ()) f)
end
