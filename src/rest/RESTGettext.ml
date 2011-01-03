
(** Gettext interface
  *)

let f_ e = 
  e^^""

let fn_ f1 f2 n =
  f1^^""

let s_ s = 
  s

let sn_ s p n =
  if n = 1 then 
    s
  else
    p

let ns_ s = 
  s

IFDEF HAS_GETTEXT THEN
include
  Gettext.Library
    (struct
       let textdomain   = "rest"
       let codeset      = None
       let dir          = None
       let dependencies = Gettext.init
     end)
ENDIF
