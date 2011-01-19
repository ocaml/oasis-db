
open OUnit
open TestCommon
open ODBCompletion

let tests = 
  let opt_string = 
    function
      | Some s -> s 
      | None -> "<none>"
  in
    "Completion" >:::
    List.map
      (fun (an, exp_pkg, exp_ver) -> 
         an >::
         (fun () ->
            let (a_pkg, a_ver) = 
              tarname_regexp 0.8 an
            in
              if !verbose then 
                begin
                  Printf.eprintf "tarname: %s; pkg: %s; ver: %s\n%!" 
                    an
                    (opt_string (value a_pkg))
                    (opt_string (value a_ver))
                end;
                assert_equal 
                  ~msg:"guess values"
                  ~printer:(fun (pkg, ver) -> 
                              (opt_string pkg)^", "^(opt_string ver))
                  (Some exp_pkg, Some exp_ver)
                  (value a_pkg, value a_ver)

         ))
      [
        "foo-0.1.0", "foo", "0.1.0";
        "bar-0.2.0", "bar", "0.2.0";
        "baz_3.0~alpha1", "baz", "3.0~alpha1";
        "sexplib310-release-5.1.0", "sexplib310", "5.1.0";
      ]
