
open OASISVersion

type fmt =
  | Sexp
  | JSON

let string_of_fmt =
  function
    | Sexp -> "sexp"
    | JSON -> "json"
                
let mime_of_fmt =
  function 
    | Sexp -> "sexp/application"
    | JSON -> "json/application"

type 'a t =
    {
      to_sexp: 'a -> Sexplib.Sexp.t;
      of_sexp: Sexplib.Sexp.t -> 'a;
      to_json: 'a -> Yojson.Basic.json;
      of_json: Yojson.Basic.json -> 'a;
    }

open Sexplib.Conv

let string = 
  {
    to_sexp = sexp_of_string;
    of_sexp = string_of_sexp;
    
    to_json = (fun str -> `String str);

    of_json = 
      (function 
         | `String str -> str
         | e ->
             (* TODO: real error *)
             assert(false));
  }

let list t = 
  {
    to_sexp = sexp_of_list t.to_sexp;
    of_sexp = list_of_sexp t.of_sexp;

    to_json = 
      (fun lst ->
         `List (List.map t.to_json lst));

    of_json = 
      (function
         | `List lst ->
             List.map t.of_json lst
         | e ->
             (* TODO: real error *)
             assert(false));
  }

let wrap conv_to conv_of t = 
  {
    to_sexp = (fun v -> t.to_sexp (conv_to v));
    of_sexp = (fun s -> conv_of (t.of_sexp s));

    to_json = (fun v -> t.to_json (conv_to v));
    of_json = (fun s -> conv_of (t.of_json s));
  }

let version = 
  wrap string_of_version version_of_string string
