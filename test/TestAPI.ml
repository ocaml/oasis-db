
open OUnit
open TestCommon

let tests = 
  "API" >::
  bracket_oasis_db
    (* Pre start *)
    ignore

    (* Main *)
    (fun ocs ->
       let base_url = 
         ocs.ocs_base_url^"api"
       in
       let ctxt = 
         !odb 
       in

       let lst = 
         ODBREST.Pkg.list ~ctxt base_url ()
       in
       let _lst' = 
         List.map 
           (fun pkg ->
              pkg,
              ODBREST.PkgVer.latest ~ctxt base_url pkg)
           lst
       in
         todo "Need to upload first";
         assert(lst <> []);
         ())

    (* Post stop *)
    ignore
