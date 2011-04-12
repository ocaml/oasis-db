
open Lwt
open FileUtil
open ODBRepository

module FS = ODBFilesystem

type ctxt = 
    {
      cli_odb:       ODBContext.t;
      cli_repos:     (ODBRepository.t * ODBSync.remote) list;
      cli_cache_dir: string;
      cli_ini:       Inifiles.inifile;
    }

let output_msgs odb msgs = 
  let ctxt = odb in
    Lwt_list.iter_s
      (function
         | `Error msg ->
             ODBMessage.error ~ctxt "%s" msg
         | `Warning msg ->
             ODBMessage.error ~ctxt "%s" msg)
      msgs

let context_lwt ?(error_repos=true) () = 
  (* Create the context *)
  let ctxt = 
    ODBContext.of_oasis !BaseContext.default
  in

  (* Gather configuration files specification *)
  let spec = 
    ODBRepository.ini_repo_specs
  in

  (* Read configuration files *)
  let default_config = 
    FilePath.make_filename
      [XDGBaseDir.Config.user_dir (); "oasis"; "oasis.ini"]
  in

  let () = 
    mkdir ~parent:true (FilePath.dirname default_config);
    if not (Sys.file_exists default_config) then
      touch default_config
  in

  let other_config_files =
    List.fold_left
      (fun acc dn -> 
         find 
           (And (Is_file, Has_extension "ini"))
           (FilePath.concat dn "oasis")
           (fun acc e -> e :: acc)
           acc)
      []
      (XDGBaseDir.Config.all_dirs ~exists:true ());
  in
  let other_config_files =
    List.filter 
      (fun e ->
         FilePath.compare e default_config <> 0)
      (List.rev other_config_files)
  in

  (* Read the inifiles *)
  let ini = 
    new InifilesExt.inifiles ~spec default_config other_config_files
  in

  (* Build repositories *)
  let cache_dir =
    FilePath.make_filename
      [XDGBaseDir.Cache.user_dir (); "oasis"]
  in

  let msgs, repos = 
    ODBRepository.load cache_dir ini
  in

  let one_repo repo = 
    let dn_repo = 
      in_cache_repos cache_dir repo
    in
    let repo_fs = 
      mkdir ~parent:true dn_repo; 
      new FS.std_rw dn_repo
    in
      ODBSync.create ~ctxt repo_fs
      >|= fun sync ->
      repo, new ODBSync.remote sync repo.repo_dist_uri
  in
    
    begin
      if error_repos then
        output_msgs ctxt msgs
      else
        return ()
    end
    >>= fun () ->
    Lwt_list.map_s one_repo repos
    >|= fun repos ->
    {
      cli_odb       = ctxt; 
      cli_repos     = repos;
      cli_cache_dir = cache_dir;
      cli_ini       = ini;
    }
