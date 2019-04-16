open Syntax

type env = (name * expr) list

let rec reduce (env : env) : expr -> expr = function
  | Var x ->
    (match List.find_all (fst >> (=) x) env with
    | [(_, t)] -> t
    | _ -> Var x)
  | App (f, x) ->
    let (Lam (arg, body)) = reduce env f
    in reduce ((arg, reduce env x) :: env) body
  | Lam (a, b) -> Lam (a, reduce env b)
