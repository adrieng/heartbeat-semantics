type t =
  | Node of int
  | Seq of t * t
  | Par of t * t

let empty =
  Node 0

let work ~overhead g =
  let rec loop g =
    match g with
    | Node i ->
       i
    | Seq (g1, g2) ->
       loop g1 + loop g2
    | Par (g1, g2) ->
       overhead + loop g1 + loop g2
  in
  loop g

let span ~overhead g =
  let rec loop g =
    match g with
    | Node i ->
       i
    | Seq (g1, g2) ->
       loop g1 + loop g2
    | Par (g1, g2) ->
       overhead + max (loop g1) (loop g2)
  in
  loop g

let rec print fmt g =
  match g with
  | Node i ->
     Format.fprintf fmt "[%d]" i
  | Seq (g1, g2) ->
     Format.fprintf
       fmt "@[%a;@ %a@]"
       print g1
       print g2
  | Par (g1, g2) ->
     Format.fprintf
       fmt "@[%a@ || %a@]"
       print g1
       print g2

let print_graphviz ?(overhead = 0) ?(title = "") fmt g =
  let edge s t =
    Format.fprintf fmt "@ %d -> %d@\n" s t
  in

  let node ?(label = "") ?(shape = "ellipse") i =
    Format.fprintf fmt "@ %d [label=\"%s\", shape=%s]@\n" i label shape
  in

  let fork i = node ~label:(string_of_int overhead) ~shape:"triangle" i in

  let join i = node ~shape:"invtriangle" i in

  let rec aux s g =
    match g with
    | Node i ->
       node ~label:(string_of_int i) s;
       s
    | Seq (g1, g2) ->
       let t1 = aux s g1 in
       let t2 = aux (t1 + 1) g2 in
       edge t1 (t1 + 1);
       t2
    | Par (g1, g2) ->
       let t1 = aux (s + 1) g1 in
       let t2 = aux (t1 + 1) g2 in
       fork s;
       join (t2 + 1);
       edge s (s + 1);
       edge s (t1 + 1);
       edge t1 (t2 + 1);
       edge t2 (t2 + 1);
       t2 + 1
  in

  Format.fprintf fmt "@[digraph {@\n";
  ignore (aux 0 g);
  Format.fprintf fmt "@\n";
  Format.fprintf fmt "labelloc = \"t\";@\n";
  Format.fprintf fmt "label = \"%s\\nwork: %d\\nspan:%d\";@\n"
                 title
                 (work ~overhead g)
                 (span ~overhead g)
  ;
  Format.fprintf fmt "@]}@."
