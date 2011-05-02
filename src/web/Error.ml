
(** Error management
   
    @author Sylvain Le Gall
  *)

open Template
open Common
open XHTML.M
open Lwt
open ODBGettext
open ExtLib

let html str = 
  p ~a:[a_class ["error"]] [pcdata str]

let () =

  let error_template ?extra_headers ?code ~sp lst = 
    let accept = 
      let ri = 
        Eliom_sessions.get_ri sp
      in
        match Lazy.force ri.Ocsigen_extensions.ri_accept with
          | (fst, _, _) :: _ ->
              fst
          | [] ->
              None, None
    in
      match accept with 
        | Some "text", Some "plain" ->
            begin
              Eliom_predefmod.Text.send ?code ~sp
                ((String.concat " " lst), "text/plain")
            end

        | _, _ ->
            begin
              catch 
                (fun () -> 
                   Context.get ~sp () 
                   >>= fun ctxt ->
                   Session.action_box ~ctxt sp)
                (fun e ->
                   return 
                     (html
                        (Printf.sprintf 
                           (f_ "Cannot get account box: %s")
                           (ODBMessage.string_of_exception e))))
              >>= fun account_box ->
              Eliom_predefmod.Xhtml.send ?code ~sp
                (template_skeleton
                   ~sp 
                   ~title:(OneTitle (s_ "Error"))
                   ~div_id:"error_page"
                   ?extra_headers
                   account_box
                   (List.map html lst))
            end
  in

  let backtrace e =
    ODBMessage.error "Exception: %s; backtrace: %s" 
      ~ctxt:(Context.get_odb ())
      (Printexc.to_string e)
      (Printexc.get_backtrace ())
  in

  Eliom_services.set_exn_handler
    (fun sp e -> 
       match e with
         | Eliom_common.Eliom_404 as e ->
             raise e

         | Eliom_common.Eliom_Wrong_parameter ->
             error_template ~sp ~code:400
               [s_ "Wrong parameters"]

         | RequiresAuth ->
             error_template ~sp ~code:401
               [s_ "You need to be logged in to see this page."]

         | StateTransitionNotAllowed ->
             error_template ~sp ~code:405
               [s_ "This state transition is not allowed."]

         | Timeout msg ->
             (* TODO: error code *)
             error_template ~sp
               ~extra_headers:
               [meta
                  ~a:[a_http_equiv "refresh"] 
                  ~content:(string_of_int (int_of_float Conf.timeout_delay))
                  ()]
               [Printf.sprintf
                  (f_ "%s This page will be refreshed in %.0f seconds.")
                  msg Conf.timeout_delay]

         | Failure str ->
             backtrace e
             >>= fun () ->
             error_template ~sp ~code:500 [str]

         | e -> 
             backtrace e
             >>= fun () ->
             error_template ~sp ~code:500 ["Unmanaged error, retry later and \
                                            contact system administrator if it happens again."])
