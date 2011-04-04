
(** Generic operation on sexp 

    @author Sylvain Le Gall
  *)

type ('a, 'b, 'c, 'd) v = 
  | V1 of 'a
  | V2 of 'b
  | V3 of 'c
  | V4 of 'd

module type OPERATION =
sig
  val upgrade: ...
  val vt_of_sexp: ...
  val current: 'a 
end

module Make (Op: OPERATION) =
struct

  (** Load from file *)
  let from_file =
    sexp_load Op.vt_of_sexp upgrade

  (** Dump to file *)
  let to_file =
    sexp_dump sexp_of_vt (fun t -> V1 t)

  (** Load from channel *)
  let from_chn ~fn chn =
    sexp_load ~fn vt_of_sexp upgrade chn

  (** Dump to channel *)
  let to_chn ~fn chn =
    LwtExt.IO.sexp_dump ~fn sexp_of_vt (fun t -> V1 t) chn

end

