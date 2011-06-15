
(* Filesystem like datastructure 
 * @author Sylvain Le Gall
 *)

open ODBGettext

module HashString = 
  Hashtbl.Make
    (struct 
       type t = string
       let equal s1 s2 = String.compare s1 s2 = 0
       let hash = Hashtbl.hash 
     end)

type filename = string

exception ComponentDoesntExist of filename
exception ComponentIsAFile of filename
exception ComponentIsADir of filename
exception CannotReplace of filename 
exception EmptyDir of filename

let () = 
  Printexc.register_printer 
    (function
       | ComponentDoesntExist fn ->
           Some 
             (Printf.sprintf
                (f_ "Component %S doesn't exist")
                fn)
       | ComponentIsAFile fn ->
           Some
             (Printf.sprintf
                (f_ "Component %S is a file")
                fn)
       | ComponentIsADir fn ->
           Some 
             (Printf.sprintf
                (f_ "Component %S is a directory")
                fn)
       | CannotReplace fn ->
           Some
             (Printf.sprintf
                (f_ "Cannot replace component %S")
                fn)
       | EmptyDir fn ->
           Some 
             (Printf.sprintf 
                (f_ "Empty directory %S")
                fn)
       | _ ->
           None)

type ('a, 'b) t = 
  | Dir of 'a * (('a, 'b) t HashString.t)
  | File of 'b 

let explode fn = 
  ExtLib.String.nsplit fn "/" 

let implode lst =
  String.concat "/" lst

let root a = 
  Dir (a, HashString.create 13)

let find_low ?not_found t fn f =
  let rec find_low' (parent_opt, acc) t lst = 
    match lst with 
      | [] ->
          f parent_opt t

      | hd :: tl ->
          begin
            match t with 
              | Dir (_, hsh) ->
                  begin
                    try 
                      find_low' (Some (hsh, hd), hd :: acc) (HashString.find hsh hd) tl
                    with Not_found ->
                      match not_found, tl with 
                        | Some f, [] ->
                            f hsh hd
                        | _, _ ->
                            raise 
                              (ComponentDoesntExist 
                                 (implode (List.rev (hd :: acc))))
                  end

              | File _ ->
                  raise 
                    (ComponentIsAFile 
                       (implode (List.rev (hd :: acc))))
          end
  in
    find_low' (None, []) t (explode fn)

let add t fn node =
  find_low
    ~not_found:
    (fun parent_hsh basename ->
       HashString.add parent_hsh basename node)
    t
    fn
    (fun parent_opt node' ->
       match parent_opt with 
         | Some (parent_hsh, basename) ->
             begin
               match node', node with 
                 | Dir _, _ ->
                     raise (CannotReplace fn)
                 | File _, File _ ->
                     HashString.replace parent_hsh basename node
                 | File _, Dir _ ->
                     raise (CannotReplace fn)
             end
         | None ->
             raise (CannotReplace fn))

let add_dir t fn a = 
  add t fn (root a)

let add_file t fn b =
  add t fn (File b)

let mem t fn =
  try
    find_low t fn (fun _ _ -> true)
  with _ ->
    false

let remove t fn = 
  try 
    find_low
      t fn
      (fun parent_opt _ ->
         match parent_opt with 
           | Some (parent_hsh, basename) ->
               HashString.remove parent_hsh basename
           | None -> 
               ())
  with _ ->
    ()

let find t fn =
  find_low t fn (fun _ node -> node)

let is_dir t fn =
  try 
    match find t fn with
      | Dir _ -> true
      | File _ -> false
  with _ ->
    false

let elements t fn = 
  match find t fn with 
    | Dir (_, hsh) ->
        HashString.fold
          (fun k _ acc -> k :: acc)
          hsh []
    | File _ ->
        raise (ComponentIsAFile fn)

let find_file t fn = 
  match find t fn with
    | Dir _ -> 
        raise (ComponentIsADir fn)
    | File b ->
        b

let find_dir t fn =
  match find t fn with
    | Dir (a, _) ->
        a
    | File _ ->
        raise (ComponentIsAFile fn)
