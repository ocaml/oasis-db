
type ty = 
  | Unit 
  | String of string
  | Cross of ty *  ty

type 'a t = ty

let unit : unit t = 
  Unit

let string nm : string t = 
  String nm

let ( ** ) (p1: 'a t) (p2: 'b t) : ('a * 'b) t = 
  Cross (p1, p2)
