
open ODBTypes
open ODBMessage
open ODBGettext
open ODBFileUtil
open Inotify
open Lwt

(** {2 Lwt enabled Inotify functions} *)

let init () = 
  Lwt_unix.of_unix_file_descr (init ())

let add_watch fd fn evts = 
  add_watch (Lwt_unix.unix_file_descr fd) fn evts

let rm_watch fd wd =
  rm_watch (Lwt_unix.unix_file_descr fd) wd

let read fd =
  Lwt_unix.wrap_syscall 
    Lwt_unix.inputs 
    fd 
    (fun () -> read (Lwt_unix.unix_file_descr fd)) 


(** {2 High level functions} *)

module MapInt = 
  Map.Make
    (struct
       type t = int
       let compare = ( - )
     end)

type 'a t =
  {
    fd:   Lwt_unix.file_descr;
    wds:  string MapInt.t;
    acc:  'a;
  }

type watch_event =
  | Created of filename
  | Changed of filename
  | Deleted of filename

let string_of_watch_event ev =
  let ev, fn =
   match ev with
    | Created fn -> "Created", fn
    | Changed fn -> "Changed", fn
    | Deleted fn -> "Deleted", fn
  in
    Printf.sprintf "%s %s" ev fn

type watch_kind_event =
  | Dir of watch_event
  | File of watch_event

let string_of_watch_kind_event ev =  
  let knd, e =
    match ev with 
    | Dir e  -> "Dir",  e 
    | File e -> "File", e
  in
    Printf.sprintf "%s(%s)" knd (string_of_watch_event e)

let filename_of_wd wd t =
  try 
    MapInt.find (int_of_wd wd) t.wds
  with Not_found ->
    failwith (s_ "Unable to find filename of a wd")

let monitor_low ~ctxt ~recurse monitor topdir acc = 

  let protect f a =
    try 
      return (f a)
    with e ->
      fail e 
  in

  let unwatch_dir fn t =
    if recurse then
      return t
    else
      return t
  in

  let watch_dir fn t = 
    let watch_dir_aux fn t =
      try 
        let wd = 
          add_watch t.fd fn [S_Create; S_Delete; S_Close_write; S_Delete_self]
        in
          debug ~ctxt (f_ "Now watching directory '%s'") fn
          >>= fun () ->
          return {t with wds = MapInt.add (int_of_wd wd) fn t.wds}
          >>= fun t ->
          monitor (Dir (Created fn)) t.acc
          >>= fun acc ->
          monitor (Dir (Changed fn)) t.acc
          >>= fun acc ->
          return {t with acc = acc}
      with Inotify.Error (func, err) ->
        fail
          (Failure
            (Printf.sprintf 
              (f_ "Inotify.%s error: %s")
              func fn
              (* TODO: Inotify error should be unix
              (Unix.error_message err)*)))
    in

    if Sys.file_exists fn && Sys.is_directory fn then
      begin
        if recurse then 
          ODBFileUtil.fold_fs 
            (fun ev t ->
              match ev with
              | PreDir fn ->
                  watch_dir_aux fn t

              | PostDir _ ->
                  return t
              
              | ODBFileUtil.File fn ->
                  monitor (File (Created fn)) t.acc
                  >>= fun acc ->
                  monitor (File (Changed fn)) t.acc
                  >>= fun acc ->
                  return {t with acc = acc})
            fn t
        else
          watch_dir_aux fn t
          >>= fun t ->
          ODBFileUtil.fold_dir
            (fun full _ t ->
              monitor (File (Created full)) t.acc
              >>= fun acc ->
              monitor (File (Changed full)) t.acc
              >>= fun acc ->
              return {t with acc = acc})
            fn t
      end
    else
      begin
        error ~ctxt 
          (f_ "Filename '%s' is not a directory, cannot watch it.")
          fn 
        >>= fun () ->
        return t
      end
  in

  let run t = 

    let contains evs lst =
      List.fold_left
        (fun acc ev ->
           acc && (List.mem ev lst))
        true
        evs
    in

    let rec run_aux t = 
      if t.wds <> MapInt.empty then
        begin
          read t.fd 
          >>=
          Lwt_list.fold_left_s
          (fun t (wd, evs, i32, fn_opt) ->
            let ident _ i =
              return i
            in
            let res = 
              List.fold_left
                (fun res (conds, pre, f_ev) ->
                  if res = None && contains evs conds then
                    begin
                      Some 
                        (fun () ->
                          begin
                            match fn_opt with 
                              | Some fn ->
                                  protect (filename_of_wd wd) t
                                  >>= fun dn ->
                                  return (Filename.concat dn fn)
                              | None ->
                                  fail (Failure "Creating directory/file without name")
                          end
                          >>= fun fn ->
                          begin
                            pre fn t
                            >>= fun t ->
                            monitor (f_ev fn) t.acc
                            >>= fun acc ->
                            return {t with acc = acc}
                          end)
                    end
                  else
                    begin
                      res
                    end)
                None
                [
                  (* Events to watch *)
                  [Create; Isdir],
                  (fun fn t ->
                    if recurse then
                      watch_dir fn t
                    else 
                      return t),
                  (fun fn -> Dir (Created fn));

                  [Delete; Isdir],
                  unwatch_dir,
                  (fun fn -> Dir (Deleted fn));

                  [Create],
                  ident,
                  (fun fn -> File (Created fn));

                  [Delete],
                  ident,
                  (fun fn -> File (Deleted fn));

                  [Close_write],
                  ident,
                  (fun fn -> File (Changed fn));

                  [Delete_self],
                  unwatch_dir,
                  (fun fn -> Dir (Deleted fn));
                ]
            in
              begin
                try 
                  debug ~ctxt
                    (f_ "Received inotify: %i (%s), [%s], %ld, %s")
                    (int_of_wd wd) (filename_of_wd wd t)
                    (String.concat "; " (List.map string_of_event evs))
                    i32
                    (match fn_opt with 
                      | Some fn -> (Printf.sprintf "Some '%s'" fn)
                      | None -> "None")
                with e ->
                  fail e
              end
              >>= fun () ->
              match res with 
                | Some f ->
                    debug ~ctxt (f_ "Processing inotify")
                    >>= 
                    f 
                | None ->
                    debug ~ctxt (f_ "Cannot process inotify")
                    >>= fun () ->
                    return t)
          t
          >>= 
          run_aux
        end

      else
        begin
          debug ~ctxt (f_ "Stop monitoring '%s'") topdir
          >>= fun () ->
          return t.acc
        end
    in

      run_aux t
  in

  let t = 
    {
      fd  = init ();
      wds = MapInt.empty;
      acc = acc;
    }
  in
    watch_dir topdir t
    >>= 
    run

let monitor_dir ~ctxt f fn acc = 
  monitor_low 
    ~ctxt ~recurse:false
    (fun ev acc ->
      match ev with 
      | Dir _ -> return acc
      | File e -> f e acc)
    fn acc
    
let monitor_fs ~ctxt f fn acc = 
  monitor_low 
    ~ctxt ~recurse:true
    f fn acc


