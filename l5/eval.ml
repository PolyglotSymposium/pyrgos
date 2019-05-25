open Util
open Syntax

type env = (symbol * expr) list

exception UnboundVariable of symbol

let rec reduce (env : env) : expr -> expr = function
  | Symbol x ->
    (match List.find_all (fst >> (=) x) env with
    | [(_, e)] -> e
    | _ -> Symbol x)
  | Appl (f, x) ->
    let reducedF = reduce env f in
    let reducedX = reduce env x in
    (match reducedF with
    | Lambda (param, body) ->
      reduce ((param, reducedX) :: env) body
    | f' -> Appl (f', reducedX))
  | Case (x, y, a, b) ->
    reduce env (
      match reduce env x with
      | Integer x' -> if x' = y then a else b
      | Symbol v -> raise (UnboundVariable v)
      | _ -> b
    )
  | IsNil (x, a, b) ->
    reduce env (
      match reduce env x with
      | Nil -> a
      | Symbol v -> raise (UnboundVariable v)
      | _ -> b
    )
  | Cons (a, b) -> Cons (reduce env a, reduce env b)
  | Uncons (x, f, a) ->
    reduce env (
      match reduce env x with
      | Cons (m, n) -> (Appl (Appl (f, m), n))
      | Symbol v -> raise (UnboundVariable v)
      | _ -> a
    )
  (* Integers, Lambda and Nil cannot be reduced *)
  | x -> x

let eval (env : env) : toplvl -> env*(expr option) =
  function
  | Define (var, value) -> ((var, reduce env value) :: env, None)
  | Expr e -> (env, Some (reduce env e))
  | Blank -> (env, None)
