type lit =
  | Int of int
  | Bool of bool

type op =
  | Add | Sub | Mul | Div
  | Le | Lt | Ge | Gt
  | Proj of bool

type t =
  | Var of Ident.t
  | Fun of Ident.t * t
  | App of t * t
  | Pair of t * t
  | Par of t * t
  | Let of Ident.t * t * t
  | Rec of Ident.t * Ident.t * t
  | If of t * t * t
  | Lit of lit
  | Op of op

let rec print_lit fmt lit =
  match lit with
  | Int i ->
     Format.fprintf fmt "%d" i
  | Bool b ->
     Format.fprintf fmt "%b" b

let rec print_op fmt op =
  let table =
    [
      Add, "+"; Sub, "-"; Mul, "*"; Div, "/";
      Le, "<="; Lt, "<"; Ge, ">="; Gt, ">";
      Proj false, "fst"; Proj true, "true";
    ]
  in
  Format.fprintf fmt "%s" (List.assoc op table)

(* TODO less parenthesis *)
let rec print fmt e =
  match e with
  | Var x ->
     Ident.print fmt x
  | Fun (x, body) ->
     Format.fprintf
       fmt "(fun %a -> %a)"
       Ident.print x
       print body
  | App (e1, e2) ->
     Format.fprintf
       fmt "(@[%a@ %a@])"
       print e1
       print e2
  | Pair (e1, e2) ->
     Format.fprintf
       fmt "(@[%a,@ %a@])"
       print e1
       print e2
  | Par (e1, e2) ->
     Format.fprintf
       fmt "(@[%a@ || %a@])"
       print e1
       print e2
  | Let (x, e1, e2) ->
     Format.fprintf
       fmt "@[<v>let %a = %a in@ %a@]"
       Ident.print x
       print e1
       print e2
  | Rec (f, x, body) ->
     Format.fprintf
       fmt "(@[rec@ (@[%a,@ %a@]).@ %a@])"
       Ident.print f
       Ident.print x
       print body
  | If (e1, e2, e3) ->
     Format.fprintf
       fmt "(@[if %a@ then %a@ else %a@])"
       print e1
       print e2
       print e3
  | Lit lit ->
     print_lit fmt lit
  | Op op ->
     print_op fmt op

let fst e =
  App (Op (Proj false), e)

let snd e =
  App (Op (Proj true), e)
