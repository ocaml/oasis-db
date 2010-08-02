
open Lwt_log

type t = 
  {
    section: section;
    logger:  logger;
  }


let default = 
  {
    section = Section.make "oasis-db";
    logger  = !default;
  }

let to_oasis ctxt =
  (* TODO: redirect context to logger *)
  !OASISContext.default

let sub ctxt nm =
  {ctxt with 
    section = 
      Section.make
        ((Section.name ctxt.section)^"/"^nm)}
