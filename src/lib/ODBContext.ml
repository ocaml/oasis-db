
open Lwt_log
open ODBTypes

type t = 
  {
    section:           section;
    logger:            logger;
    tar:               program;
    unzip:             program;
    storage_dir:       dirname;
    dist_dir:          dirname;
    incoming_dir:      dirname;
    tmp_dir:           dirname;
    min_running_time:  seconds;
  }


let default ?logger storage_dir = 
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
      storage_dir      = storage_dir;
      dist_dir         = FilePath.concat storage_dir "dist";
      incoming_dir     = FilePath.concat storage_dir "incoming";
      tmp_dir          = FilePath.concat storage_dir "tmp";
      min_running_time = 300.0;
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
