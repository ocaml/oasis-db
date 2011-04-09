
open SubCommand
open ODBGettext
open ODBCLICommon
open FileUtil
open Lwt

let download_policy = ref `Minimal 

let main () = 
  Lwt_unix.run 
    (ODBCLICommon.context_lwt () 
     >>= fun ctxt -> 
     ctxt.cli_repo_sync#update
     >>= fun () ->
     (* Apply download policy *)
     let policy fn = 
       match !download_policy with 
         | `Minimal ->
             let bn = FilePath.basename fn 
             in
               bn = "storage.sexp" ||
               bn = "_oasis"
         | `All ->
             true
     in
       FS.fold
         (fun fne acc ->
            match fne with 
              | `File fn ->
                  if policy fn then
                    return (fn :: acc) 
                  else
                    return acc
              | _ ->
                  return acc)
         ctxt.cli_repo_sync "" []
       >>= fun lst ->
       Lwt_list.iter_s
         (fun fn ->
            FS.with_file_in 
              ctxt.cli_repo_sync 
              fn 
              (fun _ -> return ()))
         lst)

let scmd =
  {(SubCommand.make 
      "update"
      (s_ "Retrieve new packages")
      ODBCLIData.update_mkd
      main)
     with 
         scmd_specs =
           [
             "-download-policy",
             Arg.Symbol 
               (["minimal"; "all"],
                fun s ->
                  download_policy :=
                  match s with 
                    | "minimal" -> `Minimal
                    | "all" -> `All
                    | s -> invalid_arg s),
             "[minimal|all] Define what contents to download."
           ]}

let () =
  SubCommand.register scmd
