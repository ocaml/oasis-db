
(** 'update' subcommand handler.
   
     Synchronize data with remote servers.

     @author Sylvain Le Gall
  *)

open SubCommand
open ODBGettext
open ODBCLICommon
open ODBRepository
open FileUtil
open Lwt

let update_repo (repo, repo_sync) = 
  (* Apply download policy *)
  let policy fn = 
    match repo.repo_download_policy with 
      | `Nothing ->
          false
      | `Minimal ->
          let bn = FilePath.basename fn 
          in
            bn = "storage.sexp" ||
            bn = "_oasis"
      | `Full ->
          true
  in
    repo_sync#update
    >>= fun () ->
    ODBVFS.fold
      (fun fne acc ->
         match fne with 
           | `File fn ->
               if policy fn then
                 return (fn :: acc) 
               else
                 return acc
           | _ ->
               return acc)
      repo_sync "" []
    >>= fun lst ->
    Lwt_list.iter_s
      (fun fn ->
         ODBVFS.with_file_in 
           repo_sync 
           fn 
           (fun _ -> return ()))
      lst

let main () = 
  Lwt_unix.run 
    (ODBCLICommon.context_lwt () 
     >>= fun ctxt -> 
     begin
       try 
         (* First pass: we update repository descriptions *)
         let msgs = 
           ODBRepository.update ctxt.cli_cache_dir ctxt.cli_ini
         in
           ODBCLICommon.output_msgs ctxt.cli_odb msgs
           >>= fun () ->
           (* Reload context *)
           ODBCLICommon.context_lwt ()
       with e ->
         fail e
     end
     >>= fun ctxt ->
     Lwt_list.iter_s update_repo ctxt.cli_repos)

let scmd =
  SubCommand.make 
    "update"
    (s_ "Retrieve new packages")
    ODBCLIData.update_mkd
    main

let () =
  SubCommand.register scmd
