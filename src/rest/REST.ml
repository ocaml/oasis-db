
open OASISVersion
open Lwt
open Sexplib
open RESTConv
open RESTGettext

(** An API call cannot be associated with a path *)
exception NoDefaultPath

type mkd = string
type string_uri = string
type string_data = string
type path_atom = string
type path = RESTConv.fmt * (path_atom list)

type status = New | Standard | Deprecated

type section_t =
    {
      sct_title: string;
      sct_help:  mkd;
      sct_order: int;
      sct_base:  path_atom list;
    }

type ('a, 'b, 'c) function_t =
    {
      fun_section:        section_t;
      fun_title:          string;
      fun_default_path:   path;
      fun_other_paths:    path list;
      fun_params:         'a RESTParams.t;
      fun_valid_versions: comparator;
      fun_status:         status;
      fun_help:           mkd;
      fun_examples:       (mkd * 'a * 'b) list;
      fun_act:            'c -> 'a -> 'b Lwt.t;
      fun_convert:        'b RESTConv.t;
    }

module SetString = Set.Make (String)

module type VERSIONS =
sig
  val current_version: OASISVersion.t 
  val past_versions:   OASISVersion.t list
end

module Make (V: VERSIONS) = 
struct 

  (* Compute possible API paths, the latest available path is returned first.
   * @raise NoDefaultPath if no path exists. 
   * @params versions list of versions to use rather than all existing versions
   * @params fmts list of format to consider 
   *)
  let mk_paths
        ?(versions=V.current_version :: V.past_versions) 
        ?(fmts=[Sexp; JSON])
        valid_versions
        section
        path = 
    (* Versions that apply *)
    let versions = 
      List.filter 
        (fun v ->
           comparator_apply 
             v 
             valid_versions)
        versions
    in
    let tail_path =
      section.sct_base @ path
    in
    let acc = 
      []
    in
    let paths =
      List.fold_left 
        (fun acc v ->
           List.fold_left 
             (fun acc f -> 
                let path = 
                  string_of_version v :: 
                  string_of_fmt f ::
                  tail_path
                in
                  (f, path) :: acc)
             acc
             fmts)
        acc
        versions
    in
      match List.rev paths with 
        | hd :: tl ->
            hd, tl
        | [] ->
            raise NoDefaultPath
        

  let registered_paths = 
    ref SetString.empty 

  (** Create an API call and check it is not a duplicate of others
    * already defined API call
    *)
  let create
        ~section
        ~title
        ~path
        ~params
        ~valid_versions
        ~help
        ~examples
        act
        convert = 

    let valid_versions =
      comparator_of_string valid_versions
    in

    let dflt, paths = 
      try 
        mk_paths valid_versions section path
      with NoDefaultPath ->
        failwith 
          (Printf.sprintf
             (f_ "API call %s doesn't match an API version")
             (String.concat "/" 
                (section.sct_base @ path)))
    in

    let paths' = 
      List.fold_left
        (fun acc path ->
           let path = 
             String.concat "/" path 
           in
             if SetString.mem path acc then 
               failwith 
                 (Printf.sprintf 
                    (f_ "API path '%s' already registered")
                    path)
             else
               SetString.add path acc)
        !registered_paths
        (List.map snd (dflt :: paths))
    in

    let status = 
      let cmp v = 
        comparator_apply v valid_versions
      in
        if not (cmp V.current_version) then
          Deprecated 
        else if (List.filter cmp V.past_versions) = [] then
          New
        else
          Standard
    in

    let () = 
      registered_paths := paths'
    in

      {
        fun_section        = section;
        fun_title          = title;
        fun_default_path   = dflt;
        fun_other_paths    = paths;
        fun_params         = params;
        fun_valid_versions = valid_versions;
        fun_help           = help;
        fun_status         = status;
        fun_examples       = examples;
        fun_act            = act;
        fun_convert        = convert;
      }
end
