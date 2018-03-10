open Misc

type value =
  | Vlit of Tree.lit
  | Vop of Tree.op
  | Vclo of Ident.t * Tree.t * env
  | Vpair of value * value

and env =
  value Ident.Env.t

type frame =
  | Fappl of env * Tree.t
  | Fappr of value
  | Fpairl of env * Tree.t
  | Fpairr of value
  | Fparl of env * Tree.t
  | Fparr of value
  | Fletl of Ident.t * env * Tree.t
  | Fif of env * Tree.t * Tree.t

type stack = frame list

type head =
  | Hclo of Tree.t * env
  | Hval of value

type state = head * stack

val print_value : value pp

val print_env : env pp

val print_frame : frame pp

val print_stack : stack pp

val print_head : head pp

val print_state : state pp

val load : ?env:env -> Tree.t -> state

exception Type_error of state

val step : state -> state

type observer = state -> unit

type selector =
  overhead:int ->
  credits:int ->
  current:stack ->
  (stack * state * stack) option

val select_none : selector

val select_eager : selector

val select_treshold_choice : choice:(int -> int) -> treshold:int -> selector

val select_treshold_outermost : treshold:int -> selector

val select_treshold_innermost : treshold:int -> selector

val select_treshold_middle : treshold:int -> selector

val select_amortized_outermost : selector

val select_amortized_innermost : selector

val select_amortized_middle : selector

val eval : ?observer:observer ->
           selector:selector ->
           overhead:int ->
           state ->
           value * Sp.t
