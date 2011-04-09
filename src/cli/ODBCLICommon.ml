
open Lwt
open FileUtil

module FS = ODBFilesystem

type ctxt = 
    {
      cli_odb:        ODBContext.t;
      cli_repo_sync:  ODBSync.remote;
    }

let context_lwt () = 
  let repo_name = "localhost" in
  let repo_url  = "http://localhost:8080/dist" in

  let repo_cache_dir =
    FilePath.make_filename
      [XDGBaseDir.Cache.user_dir (); "oasis"; "repo-"^repo_name]
  in

  let ctxt = 
    ODBContext.of_oasis !BaseContext.default
  in

  let () = 
    mkdir ~parent:true repo_cache_dir
  in

  let repo_fs =
    new FS.std_rw repo_cache_dir 
  in
    
    ODBSync.create ~ctxt repo_fs
    >>= fun sync ->
    return 
      {
        cli_odb = ctxt; 
        cli_repo_sync = new ODBSync.remote sync repo_url;
      }
