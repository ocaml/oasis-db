
open RESTConv
open Eliom_services
open Eliom_predefmod
open Eliom_parameters
open Eliom_sessions
open Xhtml
open XHTML.M
open Lwt
open OASISVersion
open REST
open RESTGettext

type help = 
 {
   help_valid_versions: comparator;
   help_section:        section_t;
   help_title:          string;
   help_txt:            mkd;
   help_examples:       (mkd * ((string_uri * string) list)) list;
   help_status:         status;
 }

let help_data =
  ref []

let register_new_service get_t base_path api_fun = 

  let get_params =
    let rec get_params' p = 
      match p with 
        | RESTParams.Unit -> 
            (* oops: need GADT here *)
            Obj.magic unit
        | RESTParams.String nm -> 
            Obj.magic (string nm)
        | RESTParams.Cross (p1, p2) ->
            Obj.magic ((get_params' p1) ** (get_params' p2))
    in
      get_params' api_fun.fun_params
  in

  (* Create one API service *)
  let register_one (fmt, path) = 
    let conv data = 
      match fmt with 
        | Sexp ->
            Sexplib.Sexp.to_string_hum 
              (api_fun.fun_convert.to_sexp data)
        | JSON ->
            Yojson.Basic.pretty_to_string
              (api_fun.fun_convert.to_json data)
    in
    let service =
      Text.register_new_service 
        ~path:(base_path @ path)
        ~get_params
        (fun sp p () ->
           get_t sp 
           >>= fun t ->
           api_fun.fun_act t p  
           >|= fun data ->
           (conv data, 
            mime_of_fmt fmt))
    in
    let examples = 
      List.map 
        (fun (help, params, data) ->
           help, 
           (fun sp -> make_string_uri ~absolute:true ~service ~sp params),
           try 
             conv data
           with _ ->
             "error")
        api_fun.fun_examples
    in
      service, examples
  in

  let (dflt_fmt, dflt) = 
    api_fun.fun_default_path
  in

  let dflt_srvc, dflt_examples =
    register_one (dflt_fmt, dflt)
  in

  let srvc_lst = 
    List.map register_one api_fun.fun_other_paths
  in

  let help = 
    (* We extract a sample of all format example *)
    let examples =
      (* Start by converting default service example *)
      List.map 
        (fun (help, fun_uri, data_str) ->
           help, [fun_uri, data_str])
        dflt_examples
    in

    let _, examples =
      (* Now look for other format and add them to 
       * examples 
       *)
      List.fold_left2
        (fun (fmts, acc) fmt examples ->
           if not (List.mem fmt fmts) then
             (fmt :: fmts),
             (* Combine examples *)
             (List.map2 
                (fun (hlp, acc) (hlp', fun_uri, data_str) -> 
                   let ex = 
                     fun_uri, data_str
                   in
                     assert (hlp = hlp');
                     hlp, (ex :: acc))
                acc 
                examples)
           else
             (fmts, acc))
        ([dflt_fmt], examples)
        (List.map fst api_fun.fun_other_paths)
        (List.map snd srvc_lst)
    in

    let examples sp = 
      List.map 
        (fun (hlp, exs) -> 
           hlp, 
           List.rev_map 
             (fun (fun_uri, data_str) ->
                fun_uri sp, data_str)
             exs)
        examples
    in

      fun sp ->
        {
          help_valid_versions = api_fun.fun_valid_versions;
          help_section        = api_fun.fun_section;
          help_title          = api_fun.fun_title;
          help_txt            = api_fun.fun_help;
          help_examples       = examples sp;
          help_status         = api_fun.fun_status;
        }
  in
    help_data := help :: !help_data;
    dflt_srvc

let help_box ~sp html_of_mkd () = 
  let help_data = 
    List.map 
      (fun f -> f sp) 
      !help_data
  in
  let all_help = 
    (* TODO: if i1 = i2 and sct <> sct' -> error *)
    List.sort 
      (fun {help_section = {sct_order = i1}} 
           {help_section = {sct_order = i2}} -> 
         i1 - i2)
      help_data
  in
  let help_one t =
    [
      h3 [pcdata (s_ t.help_title)];

      (let status, text = 
         match t.help_status with 
           | Deprecated -> 
               "deprecated",
               Printf.sprintf 
                 (f_ "Deprecated (%s)")
                 (string_of_comparator 
                    t.help_valid_versions)

           | Standard -> 
               "standard",
               Printf.sprintf
                 (f_ "Versions: %s")
                 (string_of_comparator 
                    t.help_valid_versions)

           | New ->
               "new",
               s_ "New"
       in
         div 
           ~a:[a_class ["version"; status]]
           [pcdata text])
    ]
    @
    (html_of_mkd t.help_txt)
    @
    (List.flatten
       (List.map
          (fun (hlp, exs) ->
             html_of_mkd hlp
             @
             (List.map 
                (fun (uri, data) ->
                   pre
                     [pcdata "$ curl -i ";
                      pcdata uri;
                      pcdata "\n";
                      pcdata "\n";
                      pcdata data])
                exs))
          t.help_examples))
  in
  let _, res =
    List.fold_left
      (fun (prev, acc) e ->
         let display_section = 
           match prev with 
             | Some section -> 
                 e.help_section <> section
             | None -> true
         in
         let section = 
           e.help_section 
         in
         let acc =
           if display_section then 
             List.rev_append
               (h2 [pcdata section.sct_title]
                ::
                (html_of_mkd section.sct_help))
               acc
           else
             acc
         in
         let acc =
           List.rev_append (help_one e) acc
         in
           (Some section), acc)
      (None, [])
      all_help
  in
    List.rev res
