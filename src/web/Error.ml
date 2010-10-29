
(** Error management
   
    @author Sylvain Le Gall
  *)

open Template
open Common
open XHTML.M
open Lwt
open ODBGettext

let () =
  let error_message s = 
    [p ~a:[a_class ["error"]] [pcdata s]]
  in

  let error_template ?extra_headers ?code ~sp lst = 
    catch 
      (fun () -> 
         Account.box sp)
      (fun e ->
         return 
           (error_message
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
         lst)
  in

  let backtrace acc =
    if Printexc.backtrace_status () then 
      p [pcdata (Printexc.get_backtrace ())] :: acc
    else
      acc
  in

  Eliom_services.set_exn_handler
    (fun sp e -> 
       match e with
         | Eliom_common.Eliom_404 as e ->
             raise e

         | Eliom_common.Eliom_Wrong_parameter ->
             error_template ~sp 
               (error_message 
                  (s_ "Wrong parameters"))

         | RequiresAuth ->
             error_template ~sp 
               (error_message 
                  (s_ "You need to be logged in to see this page."))

         | Timeout msg ->
             error_template ~sp
               ~extra_headers:
               [meta
                  ~a:[a_http_equiv "refresh"] 
                  ~content:"5"
                  ()] (* TODO: general conf *)
               (error_message msg)

         | Failure str ->
             error_template ~sp 
               (backtrace 
                  (error_message str))

         | e -> 
             error_template ~sp
               (backtrace 
                  (error_message
                     (Printexc.to_string e))))
