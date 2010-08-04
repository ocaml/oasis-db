
open ODBContext

type ctxt =
  {
    fake_incoming: string;
    odb:           ODBContext.t;
  }

let in_data_dir ~ctxt fn = 
  FilePath.make_filename ["test"; "data"; fn]

let in_incoming_dir ~ctxt fn = 
  FilePath.concat ctxt.odb.incoming_dir fn 

let in_dist_dir ~ctxt fn =
  FilePath.concat ctxt.odb.dist_dir fn

let odb_run ctxt f = 
  Lwt_main.run (f ~ctxt:ctxt.odb ())

