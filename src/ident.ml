type t = string

let print fmt x =
  Format.fprintf fmt "%s" x

module Env =
  struct
    include Map.Make(
                struct
                  type t = string
                  let compare = Pervasives.compare
                end
              )

    let print pp fmt env =
      Format.fprintf fmt "@[{";
      iter (fun k v -> Format.fprintf fmt " %s -> @[%a@];" k pp v) env;
      Format.fprintf fmt " }@]"
  end
