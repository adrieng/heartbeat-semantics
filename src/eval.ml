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

let rec print_value fmt v =
  match v with
  | Vlit i ->
     Tree.print_lit fmt i
  | Vop op ->
     Tree.print_op fmt op
  | Vclo (x, body, env) ->
     Format.fprintf
       fmt "(@[%a.%a@])%a"
       Ident.print x
       Tree.print body
       print_env env
  | Vpair (v1, v2) ->
     Format.fprintf
       fmt "(@[%a,@ %a@])"
       print_value v1
       print_value v2

and print_env fmt env =
  Ident.Env.print print_value fmt env

let print_frame fmt f =
  match f with
  | Fappl (env, e) ->
     Format.fprintf
       fmt "APPL(@[_,@ %a,@ %a@])"
       print_env env
       Tree.print e
  | Fappr v ->
     Format.fprintf
       fmt "APPR(@[%a,@ _@])"
       print_value v
  | Fpairl (env, e) ->
     Format.fprintf
       fmt "PAIRL(@[_,@ %a,@ %a@])"
       print_env env
       Tree.print e
  | Fpairr v ->
     Format.fprintf
       fmt "PAIRR(@[%a,@ _@])"
       print_value v
  | Fparl (env, e) ->
     Format.fprintf
       fmt "PARL(@[_,@ %a,@ %a@])"
       print_env env
       Tree.print e
  | Fparr v ->
     Format.fprintf
       fmt "PARR(@[%a,@ _@])"
       print_value v
  | Fletl (x, env, body) ->
     Format.fprintf
       fmt "LETL(@[_,@ %a,@ %a,@ %a@])"
       Ident.print x
       print_env env
       Tree.print body
  | Fif (env, e2, e3) ->
     Format.fprintf
       fmt "IF(@[_,@ %a,@ %a,@ %a@])"
       print_env env
       Tree.print e2
       Tree.print e3

let rec print_stack fmt s =
  match s with
  | [] ->
     Format.fprintf fmt "NIL"
  | f :: s ->
     Format.fprintf
       fmt "%a :: %a"
       print_frame f
       print_stack s

let print_head fmt h =
  match h with
  | Hclo (e, env) ->
     Format.fprintf
       fmt "%a@ | %a"
       Tree.print e
       print_env env
  | Hval v ->
     Format.fprintf
       fmt "%a@ | -"
       print_value v

let print_state fmt (h, k) =
  Format.fprintf
    fmt "<@[%a@ | %a@]>"
    print_head h
    print_stack k

let load ?(env = Ident.Env.empty) e =
  (Hclo (e, env), [])

exception Type_error of state

let type_error st =
  raise (Type_error st)

let apply_op st op v =
  let open Tree in
  match op, v with
  | (Add | Sub | Mul | Div),
    Vpair (Vlit (Int i1), Vlit (Int i2)) ->
     let table =
       [
         Add, ( + );
         Sub, ( - );
         Mul, ( * );
         Div, ( / );
       ]
     in
     Vlit (Int ((List.assoc op table) i1 i2))

  | (Le | Lt | Ge | Gt),
    Vpair (Vlit (Int i1), Vlit (Int i2)) ->
     let table =
       [
         Le, ( <= );
         Lt, ( <  );
         Ge, ( >= );
         Gt, ( >  );
       ]
     in
     Vlit (Bool ((List.assoc op table) i1 i2))

  | Proj b, Vpair (v1, v2) ->
     if b then v2 else v1

  | _ ->
     type_error st

let step st =
  let open Tree in
  match st with
  (* Variables *)

  | Hclo (Var x, env), k ->
     (try Hval (Ident.Env.find x env), k
      with Not_found -> raise (Type_error st))

  (* Functions *)

  | Hclo (Fun (x, body), env), k ->
     Hval (Vclo (x, body, env)), k

  (* Application *)

  | Hclo (App (e1, e2), env), k ->
     Hclo (e1, env), Fappl (env, e2) :: k

  | Hval v1, Fappl (env, e2) :: k ->
     Hclo (e2, env), Fappr v1 :: k

  | Hval v2, Fappr v1 :: k ->
     let h =
       match v1 with
       | Vclo (x, body, env) ->
          Hclo (body, Ident.Env.add x v2 env)
       | Vop op ->
          Hval (apply_op st op v2)
       | _ ->
          type_error st
     in
     h, k

  (* Sequential pairs *)

  | Hclo (Pair (e1, e2), env), k ->
     Hclo (e1, env), Fpairl (env, e2) :: k

  | Hval v1, Fpairl (env, e2) :: k ->
     Hclo (e2, env), Fpairr v1 :: k

  | Hval v2, Fpairr v1 :: k ->
     Hval (Vpair (v1, v2)), k

  (* Parallel pairs, executed sequentialy *)

  | Hclo (Par (e1, e2), env), k ->
     Hclo (e1, env), Fparl (env, e2) :: k

  | Hval v1, Fparl (env, e2) :: k ->
     Hclo (e2, env), Fparr v1 :: k

  | Hval v2, Fparr v1 :: k ->
     Hval (Vpair (v1, v2)), k

  (* Let statements *)

  | Hclo (Let (x, e1, e2), env), k ->
     Hclo (e1, env), Fletl (x, env, e2) :: k

  | Hval v, Fletl (x, env, e2) :: k ->
     Hclo (e2, Ident.Env.add x v env), k

  (* Fixpoints *)

  (* rec f -> fun x -> e => (fun x -> e){f -> (x.(rec  *)

  | Hclo (Rec (f, x, body), env), k ->
     let clo = Vclo (x, App (Rec (f, x, body), Var x), env) in
     let env = Ident.Env.add f clo env in
     Hclo (Fun (x, body), env), k

  (* If statements *)

  | Hclo (If (e1, e2, e3), env), k ->
     Hclo (e1, env), Fif (env, e2, e3) :: k

  | Hval v, Fif (env, e2, e3) :: k ->
     let e =
       match v with
       | Vlit (Bool true) ->
          e2
       | Vlit (Bool false) ->
          e3
       | _ ->
          type_error st
     in
     Hclo (e, env), k

  | Hclo (Lit lit, _), k ->
     Hval (Vlit lit), k

  | Hclo (Op op, _), k ->
     Hval (Vop op), k

  (* Finished computation *)

  | Hval v, [] ->
     st

type observer = state -> unit

type selector =
  overhead:int ->
  credits:int ->
  current:stack ->
  (stack * state * stack) option

let select_none ~overhead ~credits ~current =
  None

let select_eager ~overhead ~credits ~current =
  match current with
  | Fparl (env, e2) :: k ->
     Some ([], load ~env e2, k)
  | _ ->
     None

let rec extract_nth_fparl n k =
  match k with
  | [] ->
     invalid_arg "extract_nth_fparl: list too short"

  | ((Fparl (env, e2)) as f) :: k ->
     if n = 0
     then [], load ~env e2, k
     else
       let k1, st, k = extract_nth_fparl (n - 1) k in
       f :: k1, st, k

  | f :: k ->
     let k1, st, k = extract_nth_fparl n k in
     f :: k1, st, k

let fparl_count k =
  let open List in
  length @@ filter (fun f -> match f with Fparl _ -> true | _ -> false) k

let select_treshold_choice ~choice ~treshold ~overhead ~credits ~current =
  let count = fparl_count current in
  if credits >= treshold && count > 0
  then Some (extract_nth_fparl (choice count) current)
  else None

let select_treshold_outermost =
  select_treshold_choice ~choice:(fun count -> count - 1)

let select_treshold_innermost =
  select_treshold_choice ~choice:(fun _ -> 0)

let select_treshold_middle =
  select_treshold_choice ~choice:(fun count -> count / 2)

let select_amortized_outermost ~overhead =
  select_treshold_outermost ~treshold:overhead ~overhead

let select_amortized_innermost ~overhead =
  select_treshold_innermost ~treshold:overhead ~overhead

let select_amortized_middle ~overhead =
  select_treshold_middle ~treshold:overhead ~overhead

let eval ?(observer = fun _ -> ()) ~selector ~overhead st =
  let rec loop credits st =
    observer st;
    match st with
    | Hval v, [] ->
       v, Sp.Node credits

    | h, k ->
       begin match selector ~overhead ~credits ~current:k with
       | None ->
          loop (credits + 1) (step st)
       | Some (k1, st, k) ->
          let v1, g1 = loop 0 (h, k1) in
          let v2, g2 = loop 0 st in
          let v, g = loop 0 (Hval (Vpair (v1, v2)), k) in
          v, Sp.(Seq (Node credits, (Seq (Par (g1, g2), g))))
       end
  in
  loop 0 st
