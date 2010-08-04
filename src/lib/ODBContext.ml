
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


let default storage_dir = 
  {
    section          = Section.make "oasis-db";
    logger           = !default;
    tar              = "tar";
    unzip            = "unzip";
    storage_dir      = storage_dir;
    dist_dir         = FilePath.concat storage_dir "dist";
    incoming_dir     = FilePath.concat storage_dir "incoming";
    tmp_dir          = FilePath.concat storage_dir "tmp";
    min_running_time = 300.0;
  }

let to_oasis ctxt =
  (* TODO: redirect context to logger *)
  !OASISContext.default

let sub ctxt nm =
  {ctxt with 
    section = 
      Section.make
        ((Section.name ctxt.section)^"/"^nm)}
