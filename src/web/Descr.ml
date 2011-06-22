
(** Export a description of the repository 
  *)

open Lwt
open ODBRepository
open Eliom_parameters
open Eliom_predefmod.Xhtml
open XHTML.M

let descr_sexp = 
  Eliom_predefmod.Text.register_new_service
    ~path:["descr.sexp"]
    ~get_params:unit
     (fun sp () () ->
        Context.get ~sp () 
        >|= fun ctxt ->
        let t = 
          {
            repo_name            = "dev-localhost";
            repo_long_name       = Some "Development version on localhost";
            repo_description     = None;
            repo_priority        = 500;
            repo_dist_uri        = 
              string_of_uri 
                (OCAWeb.Redirect.rewrite 
                   ~ctxt:ctxt.Context.ocaw
                   sp
                   (make_uri ~absolute:true ~service:Dist.dist ~sp []));
            repo_incoming_uri    = None;
            repo_api_uri         = 
              Some 
                (string_of_uri 
                   (OCAWeb.Redirect.rewrite
                      ~ctxt:ctxt.Context.ocaw
                      sp
                      (make_uri ~absolute:true ~service:API.api_help ~sp ())));
            repo_download_policy = `Minimal;
          }
        in
          ODBRepository.content t)

