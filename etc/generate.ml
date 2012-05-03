
let topidr = Filename.dirname (Sys.getcwd ())
let dev_vars = 
  [
    "port", "8080";
    "logdir", "tmp";
    "ocsidb", "tmp/ocsidb";
    "command_pipe", "tmp/ocsigen_command";
    "upload_dir", "tmp/";
    "incoming_dir", "data/incoming";
    "dist_dir", "data/dist";
    "mkd_dir", "mkd";
    "role_admin", "102";
    "db_file", "data/db.sql";
    "static_dir", "/home/groups/oasis/ocsigen/dev/static";
    "ocamlcore_api_block",
    "<stub>false</stub>
     <ignore-ip>true</ignore-ip>
     <redirect replace=\"192.168.0.4:8080\" by=\"oasis.ocamlcore.org/dev\"/>
     <ini>etc/ocamlcore-api.ini</ini>";
  ]

let local_vars = 
  [
    "port", "8080";
    "logdir", "tmp";
    "ocsidb", "tmp/ocsidb";
    "command_pipe", "tmp/ocsigen_command";
    "upload_dir", "tmp/";
    "incoming_dir", "$topdir/test/data/storage/incoming";
    "dist_dir", "$topdir/test/data/storage/dist";
    "mkd_dir", "$topdir/src/web/mkd/";
    "role_admin", "0";
    "db_file", "$topdir/test/data/storage/db.sql";
    "static_dir", "$topdir/src/web/static";
    "ocamlcore_api_block",
    "<stub>true</stub>
     <base-path>ocamlcore-api/stub</base-path>";
  ]

let () = 
  let vars = ref [] in
  let output = ref None in
  let tmpl_buf = Buffer.create 1024 in
  let () = 
    Arg.parse 
      [
        "-dev",
        Arg.Unit (fun () -> vars := dev_vars @ !vars),
        " load development variables.";

        "-local",
        Arg.Unit (fun () -> vars := local_vars @ !vars),
        " load local variables.";
(*
        "-prod",
        Arg.Unit (fun () -> vars := prod_vars @ !vars),
        " load production variables.";
 *)

        "-set",
        Arg.Tuple
          (let var = ref "" in
          [Arg.String (fun svar -> var := svar);
           Arg.String (fun sval -> vars := (!var, sval) :: !vars)]),
        "var+val set a variable.";

        "-o",
        Arg.String (fun fn -> output := Some fn),
        "fn output filename.";
      ]
      (fun fn ->
         let chn = open_in fn in
         let len = in_channel_length chn in
           Buffer.add_channel tmpl_buf chn len;
           close_in chn)
      "generate ocsigen.conf file."
  in
  let tmpl = Buffer.contents tmpl_buf in
  let buf = Buffer.create ((String.length tmpl) * 2) in
  let vars = 
    let rec solve vars = 
      let changed, vars = 
        List.fold_left 
          (fun (changed, acc) (vr, vl) ->
             let buf = Buffer.create (String.length vl) in
             let () = Buffer.add_substitute buf (fun nm -> List.assoc nm vars) vl in
             let vl' = Buffer.contents buf in
               (vl <> vl') || changed,  (vr, vl') :: acc)
          (false, [])
          vars
      in
      let vars = List.rev vars in
        if changed then
          solve vars 
        else 
          vars
    in
      solve !vars
  in
    Buffer.add_substitute buf (fun nm -> List.assoc nm vars) tmpl;
    match !output with 
      | Some fn ->
          let chn_out = open_out fn in
            Buffer.output_buffer chn_out buf;
            close_out chn_out
      | None ->
          Buffer.output_buffer stdout buf

