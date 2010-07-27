
type t = 
  {
    section: Lwt_log.section;
    logger:  Lwt_log.logger;
  }


let default = 
  {
    section = Lwt_log.Section.make "oasis-db";
    logger  = !Lwt_log.default;
  }
