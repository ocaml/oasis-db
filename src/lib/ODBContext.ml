
open Lwt_log
open ODBTypes

type t = 
  {
    section:           section;
    logger:            logger;
    tar:               program;
    unzip:             program;
    incoming_dir:      dirname;
  }


let default ?logger incoming_dir = 
  let logger =
    match logger with 
      | Some l -> l
      | None -> !Lwt_log.default
  in
    {
      section          = Section.make "oasis-db";
      logger           = logger;
      tar              = "tar";
      unzip            = "unzip";
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
  {ctxt with 
    section = 
      Section.make
        ((Section.name ctxt.section)^"/"^nm)}
