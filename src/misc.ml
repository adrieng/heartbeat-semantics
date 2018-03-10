type 'a pp = Format.formatter -> 'a -> unit

let print_to_string pp x =
  ignore @@ Format.flush_str_formatter ();
  Format.fprintf Format.str_formatter "%a@?" pp x;
  Format.flush_str_formatter ()

let fold_left_1 f l =
  let rec loop acc l =
    match l with
    | [] ->
       acc
  | x :: l ->
     loop (f acc x) l
  in
  match l with
  | [] ->
     invalid_arg "fold_left_1: empty list"
  | x :: l ->
     loop x l
