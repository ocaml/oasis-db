
(** Display log on an HTML page
    @author Sylvain Le Gall
  *)

open ODBGettext
open ODBContext
open Common
open Context
open Eliom_parameters
open XHTML.M
open Eliom_predefmod.Xhtml
open Lwt
open Lwt_log

type t = (section * level * string) Queue.t

(** Create a logger *)
let create () = 
  Queue.create ()

(** Share the log queue with another context *)
let set t ctxt = 
  let queue_logger = 
    Lwt_log.make
      ~output:(fun sct lvl lst ->
                 List.iter 
                   (fun str -> Queue.add (sct, lvl, str) t)
                   lst;
                 return ())
      ~close:(fun () -> return ())
  in
    (* Override logger to get data from it *)
    {ctxt with 
         odb = 
           {ctxt.odb with 
                logger = 
                  Lwt_log.broadcast 
                    [queue_logger; ctxt.odb.logger]}}

(** Copy a log queue *)
let copy t =
  Queue.copy t

let log_settings = 
  Eliom_sessions.create_volatile_table ()

let log_debug ~sp () = 
  let sessdat = 
    Eliom_sessions.get_volatile_session_data
      ~table:log_settings 
      ~sp ()
  in
    match sessdat with
      | Eliom_sessions.Data debug ->
          debug

      | Eliom_sessions.Data_session_expired
      | Eliom_sessions.No_data ->
          false

let log_debug_switch_action =
  Eliom_predefmod.Action.register_new_coservice'
    ~name:"log_debug_switch"
    ~get_params:(bool "debug")
    (fun sp debug _ ->
       Eliom_sessions.set_volatile_session_data
         ~table:log_settings
         ~sp
         debug;
       return ())


let log_box ~ctxt ~sp t = 

  let debug =
    log_debug ~sp ()
  in

  let debug_switch =
    let text = 
      if debug then
        s_ "Hide debug log"
      else
        s_ "Show debug log"
    in
      p [a log_debug_switch_action sp [pcdata text] (not debug)]
  in

  let lst = 
    Queue.fold
      (fun acc (sct, lvl, msg) ->
         if debug || lvl > Lwt_log.Debug then
           begin
             let short_nm, css_style = 
               Log.html_log_level lvl 
             in
             let line =
               tr
                 ~a:[a_class [css_style]]
                 (td [pcdata short_nm])
                 [td [pcdata msg]]
             in
               line :: acc
           end
         else
           begin
             acc
           end)
      []
      (copy t)
  in
  let log =
    match lst with 
      | hd :: tl ->
           odd_even_table hd tl
      | [] ->
          pcdata (s_ "(no log)")
  in
  let box = 
    div 
      ~a:[a_class ["log"]]
      [h3 [pcdata (s_ "Log")];
       debug_switch;
       log]
  in
    return box
