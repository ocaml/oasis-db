
open Lwt

type context =
    {
      odb:   ODBContext.t;
      role:  Account.t;
    }

let get_odb () = 
  let storage_fn = 
    (* TODO: configure *)
    FilePath.make_filename ["test"; "data"; "storage"]
  in
    (* TODO: we should find a way to "hide" this function
     * because it is only useful once (i.e. when initializing
     * background jobs
     *)
    ODBContext.default storage_fn

let get ~sp () = 
  Account.get ~sp () 
  >>= fun role ->
  return 
    {
      odb  = get_odb ();
      role = role;
    }

