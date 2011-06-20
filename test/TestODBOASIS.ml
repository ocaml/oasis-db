
(** Tests for ODBOASIS
    @author Sylvain Le Gall
  *)

open OASISTypes
open OUnit
open TestCommon
open OASISVersion

let tests = 
  "ODBOASIS" >::
  (fun () ->
     let ctxt = 
       {(ODBContext.to_oasis !odb) with OASISContext.ignore_plugins = true}
     in
     let oasis_fn =
       in_data_dir "ocamlify-0.0.1.oasis"
     in
     let oasis = 
       OASISParse.from_file ~ctxt oasis_fn
     in
     let printer_change lst =  
       String.concat ", "
         (List.map 
            (fun (field, what) ->
               match what with 
                 | `Changed (s1, s2) ->
                     Printf.sprintf "%S: %s -> %s"
                       field s1 s2
                 | `Added ->
                     Printf.sprintf "%S added" field
                 | `Missing ->
                     Printf.sprintf "%S missing" field)
            lst)
     in

     let () = 
       let oasis' = 
         {oasis with version = version_of_string "0.0.2"}
       in
         assert_equal 
           ~printer:printer_change
           ["version", `Changed ("\"0.0.1\"", "\"0.0.2\"")]
           (ODBOASIS.check_build_sensitive oasis oasis')
     in
       
     let () = 
       let oasis' = 
         {oasis with authors = []}
       in
         assert_equal
           ~printer:printer_change
           []
           (ODBOASIS.check_build_sensitive oasis oasis')
     in

     let () = 
       let oasis' =
         {oasis with version = version_of_string "0.0.2"; name = "foo"}
       in
         assert_equal
           ~printer:printer_change
           ["name",     `Changed("\"ocamlify\"", "\"foo\"");
            "version",  `Changed("\"0.0.1\"", "\"0.0.2\"")]
           (ODBOASIS.check_build_sensitive oasis oasis')
     in

     let () = 
       let oasis' =
         {oasis with 
              sections = 
                List.map 
                  (function
                     | Executable (cs, bs, exec) ->
                         Executable (cs, {bs with bs_build_depends = [FindlibPackage ("oUnit", None)]}, exec)
                     | e -> e)
                  oasis.sections}
       in
         assert_equal
           ~printer:printer_change
           ["sections.bs_build_depends", `Changed ("[]", "[OASISTypes.FindlibPackage (\"oUnit\", None)]")]
           (ODBOASIS.check_build_sensitive oasis oasis')
     in
       ())

