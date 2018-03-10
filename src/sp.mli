open Misc

type t =
  | Node of int
  | Seq of t * t
  | Par of t * t

val empty : t

val work : overhead:int -> t -> int

val span : overhead:int -> t -> int

val print : t pp

val print_graphviz : ?overhead:int -> ?title:string -> t pp
