open Sp
open Tree
open Eval

let int i = Lit (Int i)
let app = Misc.fold_left_1 (fun x y -> App (x, y))
let funs l body = List.fold_right (fun x body -> Fun (x, body)) l body

let fib =
  let fib = "fib" in
  let n = "n" in
  let x = "x" in
  let reccall dec =
    app [Var fib; app [Op Sub; Pair (Var n, int dec)]]
  in
  Rec (fib,
       n,
       If (app [Op Le; Pair (Var n, int 1)],
           int 1,
           Let (x,
                Par (reccall 1, reccall 2),
                app [Op Add; Pair (fst (Var x), snd (Var x))])))

let fib =
  app [ fib; int 12 ]

let verbose =
  ref false

let observer st =
  if !verbose then Format.printf "-> @[%a@]@." Eval.print_state st

let overhead =
  ref 0

let treshold =
  ref (!overhead)

let selectors =
  [
    "0.none", Eval.select_none;
    "1.eager", Eval.select_eager;
    "2.amo-outermost", Eval.select_amo_outermost;
    "3.amo-innermost", Eval.select_amo_innermost;
    "4.amo-middle", Eval.select_amo_middle;
  ]

let eval prefix e (suffix, selector) =
  let filen = prefix ^ suffix ^ ".dot" in
  let st = Eval.load e in
  let v, g = Eval.eval ~observer ~overhead:!overhead ~selector st in
  if !verbose then Format.printf "-> @[%a@]@." print_value v;
  let oc = open_out filen in
  let fmt = Format.formatter_of_out_channel oc in
  Sp.print_graphviz ~title:suffix ~overhead:!overhead fmt g;
  close_out oc

let _ =
  Arg.parse
    [
      "-v",
      Arg.Set verbose,
      " display computation steps";

      "-o",
      Arg.String (fun s -> overhead := int_of_string s),
      " overhead of parallelism";

      "-t",
      Arg.String (fun s -> treshold := int_of_string s),
      " amortization treshold";
    ]
    (fun s -> ())
    "Profiling interpreter for Heartbeat Scheduling"
  ;
  List.iter (eval "test." fib) selectors
