
(** 'fetch' subcommand handler
   
     Fetch package's version content

     @author Sylvain Le Gall
  *)

open Lwt
open SubCommand
open ODBGettext
open ODBPkgVer
open FileUtil
open ODBCLICommon

type filename = string

type match_cond = PkgVer | Findlib | Executable

type output = Filename | Content | Copy of filename 

let match_cond = ref PkgVer
let what = ref `Tarball
let output = ref Filename

(* TODO: dependencies *)

let anon = ref []

let main () = 
  let match_cond = !match_cond in
  let what = !what in
  let output = !output in

  let name_cond, ver_check_opt = 
    match !anon with 
      | [pkg_str; ver_cmp_str] ->
          begin
            let ver_cmp = 
              OASISVersion.comparator_of_string ver_cmp_str 
            in
              pkg_str, 
              Some 
                (fun pkg_ver ->  
                   OASISVersion.comparator_apply pkg_ver.ver ver_cmp)
          end

      | [pkg_str] ->
          begin
            pkg_str, None
          end

      | _ ->
          begin
            failwith 
              (s_ "You must define a single package and an \
                   optional version condition. For example
                   'oasis fetch foo \">= 0.1\"' or 'oasis fetch foo'")
          end
  in

  (* Look for a package's version that satisfy name and version condition
   * in a single repository 
   *)
  let one_stor stor =
    (* Extract list of package versions that match version condition *)
    ODBStorage.Pkg.elements stor
    >>= fun pkg_lst ->
    Lwt_list.fold_left_s 
      (fun acc pkg ->
         match ver_check_opt with 
           | Some f ->
               begin
                 ODBStorage.PkgVer.elements stor (`Pkg pkg) 
                 >>= fun pkg_ver_lst ->
                 begin
                   let lst = 
                     List.rev (List.filter f pkg_ver_lst)
                   in
                     match lst with
                       | hd :: _ ->
                           return (hd :: acc)
                       | [] ->
                           return acc
                 end
               end
           | None ->
               begin
                 ODBStorage.PkgVer.latest stor (`Pkg pkg)
                 >|= fun pkg_ver ->
                 pkg_ver :: acc
               end)
      []
      pkg_lst

    >>=

    (* Apply name_cond restriction *)
    Lwt_list.filter_s
      (fun pkg_ver ->
         match match_cond with 
           | PkgVer ->
               begin
                 return (pkg_ver.pkg = name_cond)
               end

           | Findlib ->
               begin
                 ODBStorage.PkgVer.oasis stor (`PkgVer pkg_ver)
                 >|= function
                   | Some oasis ->
                       begin
                         (* Build the map of findlib name to package name *)
                         let nm_fndlb_mp =
                           OASISLibrary.name_findlib_map oasis 
                         in
                           OASISUtils.MapString.mem name_cond nm_fndlb_mp
                       end
                   | None ->
                       begin
                         false
                       end
               end

           | Executable ->
               begin
                 ODBStorage.PkgVer.oasis stor (`PkgVer pkg_ver)
                 >|= function
                   | Some oasis ->
                       begin
                         try 
                           let _sct : OASISTypes.section = 
                             OASISSection.section_find 
                               (`Executable, name_cond) 
                               oasis.OASISTypes.sections
                           in
                             true
                         with Not_found ->
                           false
                       end
                   | None ->
                       begin
                         false
                       end
               end)

      >>= function
        | hd :: _ ->
            return hd
        | [] ->
            fail Not_found
  in

  let job = 
    context_lwt ()
    >>= fun ctxt ->
    Lwt_list.fold_left_s
      (fun acc (repo, fs) -> 
         catch 
           (fun () ->
              ODBStorage.create_read_only ~ctxt:ctxt.cli_odb fs
              >>= fun stor ->
              one_stor stor 
              >|= fun pkg_ver ->
              (repo, stor, fs, pkg_ver) :: acc)

           (function
              | Not_found ->
                  return acc
              | e ->
                  fail e))
      []
      ctxt.cli_repos

    >>= fun lst ->
    begin
      (* Among all repository that match name_cond and version condition
       * pick the first one
       *)
      match List.rev lst with 
        | hd :: _ ->
            (* TODO: priority of repository *)
            return hd
        | [] -> 
            fail Not_found
    end

    >>= fun (repo, stor, fs, pkg_ver) ->
    ODBStorage.PkgVer.filename stor (`PkgVer pkg_ver) what
    >>= fun fn ->
    ODBStorage.PkgVer.with_file_in
      stor (`PkgVer pkg_ver) what
      (fun chn -> 
         match output with 
           | Filename ->
               begin
                 Lwt_io.printl (fs#real_filename fn)
               end

           | Content ->
               begin
                 Lwt_io.write_chars
                   Lwt_io.stdout
                   (Lwt_io.read_chars chn)
               end

           | Copy tgt_fn ->
               begin
                 Lwt_io.chars_to_file
                   tgt_fn
                   (Lwt_io.read_chars chn)
               end)
  in
    
    (* Do the job *)
    Lwt_unix.run job


let scmd = 
  {(SubCommand.make
      "fetch"
      (s_ "Fetch a tarball from the server")
      ODBCLIData.fetch_mkd
      main)
     with 
         scmd_specs =
           [
             "-match",
             Arg.Symbol
               (["pkg-ver"; "findlib"; "executable"],
                fun str ->
                  let e = 
                    match str with
                      | "pkg-ver" -> PkgVer
                      | "findlib" -> Findlib
                      | "executable" -> Executable
                      | str ->
                          invalid_arg str
                  in
                    match_cond := e),
             " Look for a package matching either a package's version ('pkg-ver'), \
               a findlib name ('findlib') or an executable name ('executable'). With \
               'findlib' and 'pkg-ver', you can provide a version comparator to find \
               a precise package otherwise use the latest version. (default 'pkg-ver')";

             "-what",
             Arg.Symbol
               (["tarball"; "oasis"; "oasis-pristine"],
                fun str ->
                  let e =
                    match str with 
                      | "tarball" -> `Tarball
                      | "oasis" -> `OASIS
                      | "oasis-pristine" -> `OASISPristine
                      | str ->
                          invalid_arg str
                  in
                    what := e),
             " Fetch the 'tarball' or its 'oasis' file or the pristine 'oasis' file. \
               (default 'tarball')";

             "-output",
             Arg.Symbol
               (["filename"; "content"],
                fun str ->
                  let e = 
                    match str with 
                      | "filename" -> Filename
                      | "content"  -> Content
                      | str ->
                          invalid_arg str
                  in
                    output := e),
             " Output the 'filename' or its 'content'";
             
             "-copy",
             Arg.String (fun s -> output := Copy s),
             "fn Don't output anything, copy the file content to 'fn'"; 
           ];

         scmd_anon = 
           (fun s -> anon := !anon @ [s]);
  }

let () = 
  SubCommand.register scmd
