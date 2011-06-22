
open Lwt_log
open ODBTypes

type t = 
  {
    section:           section;
    section_inherit:   bool;
    logger:            logger;
    tar:               program;
    unzip:             program;
    oasis:             program;
    incoming_dir:      dirname;
  }


(* TODO: get rid of incoming_dir *)
let default ?logger incoming_dir = 
  let logger =
    match logger with 
      | Some l -> l
      | None -> !Lwt_log.default
  in
    {
      section          = Section.make "oasis-db";
      section_inherit  = false;
      logger           = logger;
      tar              = "tar";
      unzip            = "unzip";
      oasis            = "oasis";
      incoming_dir     = incoming_dir;
    }

let to_oasis ctxt =
  let printf lvl str = 
    let level = 
      match lvl with
        | `Debug -> Debug
        | `Info  -> Info 
        | `Warning -> Warning
        | `Error -> Error
    in
      Lwt.ignore_result (log ~logger:ctxt.logger ~level str)
  in
    {!OASISContext.default with 
         OASISContext.printf = printf}

let sub ctxt nm =
  let sct = 
    Section.make
      ((Section.name ctxt.section)^"/"^nm)
  in
    if ctxt.section_inherit then
      Section.set_level sct (Section.level ctxt.section);
    {ctxt with section = sct}

let of_oasis oasis_ctxt = 
  let output sct lvl lst = 
    let ctxt = oasis_ctxt in
    let msg_lvl fmt = 
      match lvl with 
        | Debug -> 
            OASISMessage.debug ~ctxt fmt 
        | Info | Notice -> 
            OASISMessage.info ~ctxt fmt
        | Warning -> 
            OASISMessage.warning ~ctxt fmt
        | Error | Fatal ->
            OASISMessage.error ~ctxt fmt
    in
      List.iter 
        (msg_lvl "(%s) %s" (Section.name sct))
        lst;
      Lwt.return ()
  in
  let logger = 
    make ~output ~close:(fun () -> Lwt.return ())
  in
  let res = 
    default ~logger "invalid"
  in
    (* Allow anything from logger, OASISMessage will apply its policy *)
    Section.set_level res.section Debug;
    {res with section_inherit = true}
