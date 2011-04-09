
open SubCommand
open ODBGettext
open FileUtil
open Lwt

module FS = ODBFilesystem

let download_policy = ref `Minimal 

let main () = 
  let repo_name = "localhost" in
  let repo_url  = "http://localhost:8080/dist" in
  let repo_download_policy = !download_policy in

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

    Lwt_unix.run 
      (ODBSync.create ~ctxt repo_fs
       >>= fun sync ->
       return (new ODBSync.remote sync repo_url)
       >>= fun repo_sync ->
       repo_sync#update
       >>= fun () ->
       (* Apply download policy *)
       let policy fn = 
         match repo_download_policy with 
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
           repo_sync "" []
         >>= fun lst ->
         Lwt_list.iter_s
           (fun fn ->
              FS.with_file_in repo_sync fn (fun _ -> return ()))
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
