
(** Cron like service
  *)

open Lwt
open XHTML.M
open ODBGettext
open Eliom_parameters
open Eliom_predefmod.Xhtml
open OASISUtils
open Context
open CalendarLib

type second = int

type freq = 
    Hourly 
  | Daily 
  | Monthly 
  | Every of second

type state =
  | NeverRun
  | StartedAt of Calendar.t
  | RunAt of Calendar.t * Calendar.Period.t

type t =
    {
      job_descr: string;
      job_freq:  freq;
      job_fun:   ctxt:Context.context -> unit -> unit Lwt.t;
      job_state: state;
    }

let jobs = ref MapString.empty 
let jobs_changed = Lwt_condition.create ()

let add_low id t =
  jobs := (MapString.add id t !jobs);
  Lwt_condition.signal jobs_changed ()

let add id descr freq f =
  add_low 
    id 
    {
      job_descr = descr;
      job_freq  = freq;
      job_fun   = f;
      job_state = NeverRun;
    }

let run ~ctxt id = 
  try 
    let t = 
      MapString.find id !jobs
    in
    let start = 
      Calendar.now () 
    in
      (* TODO: override context to be able to display logs *)
      begin
        add_low id {t with job_state = StartedAt start};
        t.job_fun ~ctxt ()
      end
      >|= fun () ->
      begin
        let delay = 
          Calendar.precise_sub (Calendar.now ()) start
        in
          add_low id {t with job_state = RunAt (start, delay)}
      end

  with Not_found ->
    ODBMessage.error ~ctxt:ctxt.odb
      (f_ "Cannot find cron job '%s'") id

let box ~ctxt ~sp () = 
  let run_now = 
    Eliom_predefmod.Action.register_new_post_coservice_for_session'
      ~sp
      ~name:"cron_run_now"
      ~post_params:(string "id")
      (fun sp () id ->
         Context.get_admin ~sp ()
         >|= fun (ctxt, _) ->
         ignore_result (run ~ctxt id))
  in
         
  let string_of_freq =
    function
      | Hourly  -> s_ "hourly"
      | Daily   -> s_ "daily"
      | Monthly -> s_ "monthly"
      | Every s -> 
          Printf.sprintf
            (fn_ "Every %d second" "Every %d seconds" s)
            s
  in

  let string_of_state = 
    function
      | NeverRun -> s_ "Never run"
      | StartedAt time -> 
          Printf.sprintf 
            (f_ "Started at %s")
            (Printer.Calendar.to_string time)
      | RunAt (time, delay) ->
          let s = 
            Time.Period.length (Calendar.Period.to_time delay)
          in
            Printf.sprintf
              (fn_ "Run at %s for %d second" "Run at %s for %d seconds" s)
              (Printer.Calendar.to_string time)
              s
  in

  let lst = 
    MapString.fold
      (fun id t acc ->
         (tr         
            (td [pcdata (s_ t.job_descr)])
            [td [pcdata (string_of_freq t.job_freq)];
             td [pcdata (string_of_state t.job_state)];
             td [pcdata (s_ "Logs");
                post_form 
                  ~sp
                  ~service:run_now
                  (fun id_nm ->
                     [p 
                        [string_button 
                           ~name:id_nm 
                           ~value:id
                           [pcdata (s_ "Run now")]]])
                  ()]])
         :: acc)
      !jobs
      []
  in
    return
      (div 
         [h1 [pcdata (s_ "Cron jobs")];
          Common.odd_even_table
            (tr 
               (th [pcdata (s_ "Description")])
               [th [pcdata (s_ "Frequency")];
                th [pcdata (s_ "State")];
                th [pcdata ""]])
            lst])

let () = 
  add "alive" 
    "Test cron is alive"
    (Every 5)
    (fun ~ctxt () ->
       ODBMessage.info ~ctxt:ctxt.Context.odb
         (f_ "Cron is alive"));
  add "alive-delay"
    "Test cron is alive (3 seconds delay)"
    (Every 10)
    (fun ~ctxt () ->
       Lwt_unix.sleep 3.0
       >>= fun () ->
       ODBMessage.info ~ctxt:ctxt.Context.odb
         (f_ "Cron is alive (3 seconds delay)"))



(** TODO: prevent running a service already running 
  *)

let start ~ctxt () =
  return ()

