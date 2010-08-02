
(** TODO: use it 
let test () = 
  List.iter
    (fun an -> 
       let lst = 
         tarname_regexp 0.8 an
       in
         Printf.printf "tarname: %s\n%!" an;
         List.iter 
           (fun (q, pkg, ver) ->
              Printf.printf "q: %f; pkg: %s; ver: %s\n%!" 
                q pkg ver)
           lst)
    [
      "foo-0.1.0";
      "bar-0.2.0";
      "baz_3.0~alpha1";
      "sexplib310-release-5.1.0";
    ]
    *)

