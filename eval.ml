open Syntax

type env = (Syntax.symbol * expr) list

let rec reduce (env : env) : expr -> expr = function
  | Appl (f, x) ->
    let (Lambda (arg, body)) = reduce env f
    in reduce ((arg, reduce env x) :: env) body
  | Lambda (a, b) -> Lambda (a, reduce env b)
  | Symbol x ->
    (match List.find_all (fst >> (=) x) env with
    | [(_, t)] -> t
    | _ -> Symbol x)
  | x -> x
