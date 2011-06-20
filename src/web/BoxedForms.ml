
(** Create page that include many small boxes that can be edited on their
    own.

    @author Sylvain Le Gall
  *)

open Lwt

type ('a, 'b) t =
    {
      display:  'a -> (bool * 'b XHTML.M.elt) Lwt.t;
      commit:   'a -> 'a Lwt.t;
      rollback: 'a -> 'a Lwt.t;
    }

let create 
      ?(rollback=(fun a -> return a)) 
      ?(commit=(fun a -> return a))
      display =
    {
      display  = display;
      commit   = commit;
      rollback = rollback;
    }

let display t a =
  t.display a

let commits lst a =
  Lwt_list.fold_left_s
    (fun a t -> t.commit a)
    lst
    a

let rollbacks lst a =
  Lwt_list.fold_left_s
    (fun a t -> t.rollback a)
    lst
    a
