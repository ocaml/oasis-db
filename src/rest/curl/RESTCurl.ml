
open ExtLib
open REST
open RESTConv

type ctxt =
    {
      base_url:    string;
      curl_socket: Curl.t;
      dbug_print:  string -> unit; 
    }

let create ?(dbug_print=ignore) base_url curl_socket =
  {
    base_url    = base_url;
    curl_socket = curl_socket;
    dbug_print  = dbug_print;
  }

let mk_fun api_fun ~ctxt params = 
  let rec chop_slash str = 
    if String.ends_with str "/" then
      chop_slash (String.rchop str)
    else
      str
  in

  let fmt, tail_path =
    api_fun.fun_default_path 
  in

  let params_str = 
    let rec get_params p = 
      match p with 
        | RESTParams.Unit ->
            Obj.magic (fun acc () -> acc)
        | RESTParams.String nm ->
            Obj.magic (fun acc str -> (nm, str) :: acc)
        | RESTParams.Cross (p1, p2) ->
            let f1 = get_params p1 in
            let f2 = get_params p2 in
              Obj.magic 
                (fun acc (v1, v2) -> 
                   let acc = f1 acc v1 in
                     f2 acc v2)
    in
      (get_params api_fun.fun_params) [] params
  in

  let url = 
      (String.concat "/" (chop_slash ctxt.base_url :: tail_path))
      ^
      (if params_str <> [] then 
         "?"
       else
         "")
      ^
      (String.concat "&"
         (List.rev_map 
            (fun (nm, vl) -> Printf.sprintf "%s=%s" nm vl)
            params_str))
  in

  let server_data =
    let c = 
      ctxt.curl_socket
    in
    let buff = 
      Buffer.create 13
    in
    let write d = 
      Buffer.add_string buff d;
      String.length d
    in

      try 
        ctxt.dbug_print (Printf.sprintf "Fetching URL '%s'" url);
        Curl.set_url c url;
        Curl.set_writefunction c write;
        Curl.set_failonerror c true;
        Curl.perform c;
        Buffer.contents buff
      with Curl.CurlException (_, curl_code, msg) ->
        failwith 
          (Printf.sprintf 
             "Error HTTP %d (curl %d, %s) when fetching %s"
             (Curl.get_httpcode c)
             curl_code msg
             url)
  in

    match fmt with 
      | Sexp ->
          api_fun.fun_convert.of_sexp 
            (Sexplib.Sexp.of_string server_data)

      | JSON ->
          api_fun.fun_convert.of_json 
            (Yojson.Basic.from_string server_data)
