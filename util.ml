(* at some point, need to learn what the proper way to do all this in OCaml
   is *)

let map_opt (f : 'a -> 'b) : 'a option -> 'b option =
  function
  | None -> None
  | Some x -> Some (f x)

let bind_opt (f : 'a -> 'b option) : 'a option -> 'b option =
  function
  | None -> None
  | Some x -> f x

let (>>) g f x = f(g(x))
let (|>) x f = f x

let unwords = String.concat " "
