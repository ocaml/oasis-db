
open OUnit
open TestCommon

let oasis_conf = 
    "<ocsigen>
       <server>
         $std_conf
         <extension findlib-package=\"pgocaml\" />
         <extension findlib-package=\"oasis\" />
         <extension findlib-package=\"sexplib\" />
         <extension findlib-package=\"inotify\" />
         <extension findlib-package=\"markdown\" />
         <extension findlib-package=\"markdown.html\" />
         <extension findlib-package=\"cameleon.rss\" />
         <extension findlib-package=\"yojson\" />
         <extension findlib-package=\"curl\" />

         <host charset=\"utf-8\" >
           <site path=\"\">
             <eliom module=\"$curdir/_build/src/rest/rest.cma\" />
             <eliom module=\"$curdir/_build/src/rest/curl/rest-curl.cma\" />
             <eliom module=\"$curdir/_build/src/rest/ocsigen/rest-ocsigen.cma\" />
             <eliom module=\"$curdir/_build/src/lib/oasis-db.cma\" />
             <eliom module=\"$curdir/_build/src/web/oasis-db-ocsigen.cma\" />
             <static dir=\"$curdir/src/web/static\" /> 
           </site>
         </host>
       </server>
     </ocsigen>"

let tests = 
  "API" >::
  bracket_ocsigen oasis_conf
    (* Pre start *)
    (fun _ -> ())

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
         assert(lst <> []);
         ())

    (* Post stop *)
    (fun _ -> ())
