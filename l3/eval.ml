open Util
open Syntax

type env = (symbol * expr) list

exception UnboundVariable of symbol
exception CannotBeApplied of expr

let rec reduce (env : env) (force : bool) : expr -> expr = function
  | Symbol x ->
    (match List.find_all (fst >> (=) x) env with
    | (_, e) :: _ -> e
    | _ -> if force then raise (UnboundVariable x) else Symbol x)
  | Appl (f, x) ->
    let reducedF = reduce env force f in
    let reducedX = reduce env force x in
    (match reducedF with
    | Lambda (param, body) ->
      (match reduce ((param, reducedX) :: env) force body with
      | Lambda (a, b) ->
        let env' = List.filter (fst >> (<>) a) env
        in Lambda (a, reduce env' false b)
      | x -> x)
    | f' -> if force then raise (CannotBeApplied f') else Appl (f', reducedX))
  | Case (x, y, a, b) ->
    reduce env force (
      match reduce env force x with
      | Integer x' -> if x' = y then a else b
      | Symbol _ -> Case (x, y, a, b)
      | _ -> b
    )
  | IsNil (x, a, b) ->
    reduce env force (
      match reduce env force x with
      | Nil -> a
      | Symbol _ -> IsNil (x, a, b)
      | _ -> b
    )
  | Cons (a, b) -> Cons (reduce env force a, reduce env force b)
  | Uncons (x, f, a) ->
    reduce env force (
      match reduce env force x with
      | Cons (m, n) -> Appl (Appl (f, m), n)
      | Symbol _ -> Uncons (x, f, a)
      | _ -> a
    )
  | x -> x (* Integers and Nil cannot be reduced *)

let eval (env : env) : toplvl -> env*(expr option) =
  function
  | Define (var, value) -> ((var, reduce env true value) :: env, None)
  | Expr e -> (env, Some (reduce env true e))
  | Blank -> (env, None)
