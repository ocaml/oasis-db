
(** Extended inifiles support
    
    @author Sylvain Le Gall
  *)

open Inifiles

(**/**)
module SetString = Set.Make(String)
(**/**)

class inifiles ?(spec=[]) fn_wrt lst = 
  (* Define a spec, where nothing is required *)
  let spec_less = 
    List.map  
      (fun sct ->
         let atrs = 
           List.map 
             (fun atr -> {atr with atr_required = false})
             sct.sec_attributes
         in
           {sct with 
                sec_required = false;
                sec_attributes = atrs})
      spec
  in
  (* List sections required, to be checked in initializer *)
  let sct_required = 
    List.fold_left
      (fun st spc ->
         if spc.sec_required then 
           SetString.add spc.sec_name st
         else
           st)
      SetString.empty spec
  in
  (* List attributes required to be checked in initializer *)
  let sct_atr_required = 
    List.fold_left 
      (fun acc sct ->
         List.fold_left
           (fun acc atr ->
              if atr.atr_required then 
                (sct.sec_name, atr.atr_name) :: acc
              else 
                acc)
           acc
           sct.sec_attributes)
      []
      spec
  in

    object (self)
      val ini_wrt = new inifile ~spec:spec_less fn_wrt
      val ini_rds = List.map (fun fn -> new inifile ~spec:spec_less fn) lst

      method getval sct atr =
        let sct_found = ref false in
          try 
            begin
              let ini = 
                List.find
                  (fun ini ->
                     try 
                       let _e : string = 
                         ini#getval sct atr
                       in 
                         true
                     with 
                       | Invalid_section _ ->
                           false
                       | Invalid_element _ ->
                           sct_found := true;
                           false
                       | e ->
                           raise e)
                  (ini_wrt :: ini_rds)
              in
                ini#getval sct atr
            end
          with Not_found ->
            begin
              if not !sct_found then
                raise (Invalid_section sct);
              raise (Invalid_element atr)
            end

      method getaval sct atr =
        let sct_found = ref false in
        let res = 
          List.flatten
            (List.map
               (fun ini ->
                  try
                    let res = 
                      ini#getaval sct atr
                    in
                      sct_found := true;
                      res
                  with 
                    | Invalid_element _ ->
                        sct_found := true;
                        []
                    | Invalid_section _ ->
                        [])
               (ini_wrt :: ini_rds))
        in
          if not !sct_found then
            raise (Invalid_section sct);
          if res = [] then
            raise (Invalid_element atr);
          res

      method setval sct atr vl =
        ini_wrt#setval sct atr vl

      method delval sct atr =
        ini_wrt#delval sct atr 

      method save ?file () = 
        ini_wrt#save ?file ()

      method iter f sct =
        let sct_found = ref false in
          List.iter
            (fun ini ->
               try 
                 ini#iter f sct;
                 sct_found := true
               with Invalid_section _ ->
                 ())
            (ini_wrt :: ini_rds);
          if not !sct_found then
            raise (Invalid_section sct)


      method sects = 
        List.flatten
          (List.map 
             (fun ini -> ini#sects) 
             (ini_wrt :: ini_rds))

      method attrs sct = 
        let sct_found = ref false in
        let res =
          List.flatten 
            (List.map 
               (fun ini ->
                  try 
                    let res = 
                      ini#attrs sct
                    in
                      sct_found := true;
                      res
                  with Invalid_section _ ->
                    [])
               (ini_wrt :: ini_rds))
        in
          if not !sct_found then
            raise (Invalid_section sct);
          res
      
      initializer 
        begin
          (* Check that required sections are present *)
          let scts = 
            List.fold_left
              (fun st sect -> SetString.add sect st)
              SetString.empty
              self#sects 
          in
          let diff = 
            SetString.diff sct_required scts 
          in
          let () =
            try 
              raise (Missing_section (SetString.choose diff))
            with Not_found ->
              ()
          in

          (* Check that required attributes are present *)
          let () = 
            List.iter 
              (fun (sct, atr) ->
                 if SetString.mem sct scts then
                   begin
                     try 
                       let _s : string = 
                         self#getval sct atr 
                       in
                         ()
                     with Invalid_element _ ->
                       raise (Missing_element atr)
                   end)
              sct_atr_required
          in
            ()
        end

    end

let () = 
  Printexc.register_printer
    (function
       | Invalid_section sct -> Some ("Invalid_section("^sct^")") 
       | Invalid_element atr -> Some ("Invalid_element("^atr^")") 
       | Missing_section sct -> Some ("Missing_section("^sct^")") 
       | Missing_element atr -> Some ("Missing_element("^atr^")") 
       | Ini_parse_error (line, fn) ->
           Some ("Ini_parse_error("^(string_of_int line)^", '"^fn^"')")
       | _ ->
           None)
