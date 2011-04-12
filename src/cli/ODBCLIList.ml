
(** 'list' subcommand handler 
   
     List available packages 

     @author Sylvain Le Gall
  *)

open SubCommand
open OASISUtils
open OASISTypes
open ODBGettext
open ODBCLICommon
open ODBPkg
open Lwt

let findlib = ref false

let main () = 
  (* TODO: make an online/offline distinction *)
  let ctxt = 
    Lwt_unix.run (context_lwt ())
  in
  let data = 
    List.fold_left
      (fun acc (repo, repo_sync) ->
         let lst =
           Lwt_unix.run
             (ODBStorage.create 
                ~ctxt:ctxt.cli_odb 
                repo_sync
                (fun ~timestamp _ -> return ())
                []
              >>= fun stor ->
              ODBStorage.Pkg.elements stor
              >>= fun lst ->
              Lwt_list.fold_left_s
                (fun acc pkg -> 
                   catch 
                     (fun () ->
                        ODBStorage.PkgVer.latest stor (`Pkg pkg)
                        >>= fun pkg_ver ->
                        ODBStorage.PkgVer.oasis stor (`PkgVer pkg_ver)
                        >|= fun oasis_opt ->
                        (pkg, pkg_ver, oasis_opt) :: acc)
                     (function
                        | Not_found ->
                            return acc
                        | e -> 
                            fail e))
                [] lst)
         in
           (repo, repo_sync, lst) :: acc)
      []
      ctxt.cli_repos
  in
  let display_mp = 
    if !findlib then
      begin
        List.fold_left
          (fun mp (_, _, lst) ->
             List.fold_left
               (fun mp (pkg, _, oasis_opt) ->
                  match oasis_opt with 
                    | Some oasis ->
                        let nm_fndlb_mp = 
                          OASISLibrary.name_findlib_map oasis
                        in
                          MapString.fold
                            (fun fndlb _ mp ->
                               MapString.add
                                 fndlb
                                 [Printf.sprintf
                                    (f_ "(version: %s)")
                                    (OASISVersion.string_of_version oasis.version);
                                  Printf.sprintf
                                    (f_ "package: %s")
                                    pkg.pkg_name]
                                 mp)
                            nm_fndlb_mp
                            mp
                    | None ->
                        mp)
               mp
               lst)
          MapString.empty
          data
      end
    else
      begin
        List.fold_left
          (fun mp (_, _, lst) ->
             List.fold_left
               (fun mp (pkg, _, oasis_opt) ->
                  match oasis_opt with 
                    | Some oasis ->
                        MapString.add pkg.pkg_name [oasis.synopsis] mp
                    | None ->
                        MapString.add pkg.pkg_name [s_ "<none>"] mp)
               mp lst)
          MapString.empty data
      end
  in

  let sz_lst = 
    let rec max_lst lst sz_lst =
      match lst, sz_lst with
        | [], [] ->
            []
        | str :: tl, sz :: sz_tl ->
            (max (String.length str) sz)
            :: (max_lst tl sz_tl)
        | [], lst ->
            lst
        | lst, [] ->
            List.map String.length lst
    in
      MapString.fold
        (fun k lst sz_lst ->
           max_lst (k :: lst) sz_lst)
        display_mp
        []
  in
  let sz_lst = List.map (( + ) 1) sz_lst in

  let print_col str sz =
    let str' = 
      String.make sz ' '
    in
      String.blit str 0 str' 0 (String.length str);
      Printf.printf "%s" str'
  in
  let rec iter_incomplete lst lst_sz =
    match lst, lst_sz with 
      | str :: tl, sz :: tl_sz ->
          print_col str sz;
          iter_incomplete tl tl_sz
      | [], _ ->
          Printf.printf "\n%!"
      | _, [] ->
          invalid_arg "iter_incomplete"
  in
    MapString.iter 
      (fun k lst ->
         iter_incomplete (k :: lst) sz_lst)
      display_mp

let scmd = 
  {(SubCommand.make
      "list"
      (s_ "List available packages on the server")
      ODBCLIData.list_mkd
      main)
     with 
         scmd_specs = 
           [
             "-findlib",
             Arg.Set findlib,
             " Display findlib name";
           ]}

let () = 
  SubCommand.register scmd
