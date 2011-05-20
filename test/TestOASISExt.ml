
(** Test extensions of the OASIS library
   
    TODO: move to OASIS
    @author Sylvain Le Gall
  *)

open TestCommon
open OUnit
open OASISTypes

let tests =
  "OASISExt" >::
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
     let sct =
       OASISSection.section_find (`Executable, "ocamlify") oasis.sections
     in
       match sct with 
         | Executable (_, bs, _) ->
             assert_equal 
               `Always 
               (OASISBuildSectionExt.installable oasis bs)
         | _ ->
             assert_failure "Expected section 'ocamlify'")



