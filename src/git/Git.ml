
(** Manipulate a git repository 
    @author Sylvain Le Gall
  *)

open Lwt

type args = string

type t =
    {
      git_exec:      args list -> Unix.process_status Lwt.t;
      git_author:    string option;
      git_work_tree: string;
    }

let create ?exec ?author ?work_tree () = 
  let exec = 
    match exec with 
      | Some f -> 
          f
      | None ->
          fun args ->
            Lwt_process.exec 
              ("git", 
               (Array.of_list ("git" :: args)))
  in
  let work_tree = 
    match work_tree with 
      | Some dn ->
          dn
      | None ->
          Sys.getcwd ()
  in
    {
      git_exec      = exec;
      git_author    = author;
      git_work_tree = work_tree;
    }


let exec t ?(init=false) args exit_code = 
  let git_dir = 
    Filename.concat t.git_work_tree ".git"
  in
  let full_args =
    "--work-tree" :: t.git_work_tree :: 
    "--git-dir" :: git_dir :: args
  in
  let pstatus_to_string =
    function
      | Unix.WEXITED n ->
          Printf.sprintf "exited with code %n" n
      | Unix.WSIGNALED n ->
          Printf.sprintf "been killed by signal %n" n
      | Unix.WSTOPPED n ->
          Printf.sprintf "been stopped by signal %n" n
  in
    if not init && 
       (not (Sys.file_exists git_dir) || 
        not (Sys.is_directory git_dir)) then
      fail
        (Failure 
           (Printf.sprintf 
              "Git directory '%s' doesn't exist."
              git_dir))
    else
      return () 
    >>= fun () ->
    t.git_exec full_args
    >>= fun pstatus ->
      if pstatus = Unix.WEXITED exit_code then
        return ()
      else
        fail
          (Failure
             (Printf.sprintf
                "Command '%s' should have exited with code %n but have %s"
                (String.concat " " ("git" :: full_args))
                exit_code
                (pstatus_to_string pstatus)))

let init t = 
  exec t ~init:true ["init"; "-q"] 0

let add fn t =
  exec t ["add"; fn] 0

let commit msg t =
  let author_args = 
    match t.git_author with 
      | Some s -> ["--author"; s]
      | None -> []
  in
    exec t ("commit" :: "-q" :: "-m" :: msg :: author_args) 0
