open Misc

type lit =
  | Int of int
  | Bool of bool

type op =
  | Add | Sub | Mul | Div
  | Le | Lt | Ge | Gt
  | Proj of bool                (* false for fst, true for snd *)

type t =
  | Var of Ident.t               (* x *)
  | Fun of Ident.t * t           (* fun x -> e *)
  | App of t * t                 (* e1 e2 *)
  | Pair of t * t                (* (e1,e2) *)
  | Par of t * t                 (* (e1 || e2) *)
  | Let of Ident.t * t * t       (* let x = e1 in e2 *)
  | Rec of Ident.t * Ident.t * t (* rec f. fun x -> e  *)
  | If of t * t * t              (* if e1 then e2 else e3 *)
  | Lit of lit
  | Op of op

val print_lit : lit pp

val print_op : op pp

val print : t pp

val fst : t -> t

val snd : t -> t
