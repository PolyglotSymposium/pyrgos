open Util
open Syntax

type env = (symbol * expr) list

exception UnboundVariable of symbol
exception CannotBeApplied of expr

let rec reduce_symbol (env : env) (force : bool) (x : symbol) : expr =
  match List.find_all (fst >> (=) x) env with
  | (_, e) :: _ -> e
  | _ -> if force then raise (UnboundVariable x) else Symbol x

and reduce_lambda (env : env) (force : bool) : expr -> expr =
  function
  | Lambda (a, b) ->
    let env' = List.filter (fst >> (<>) a) env
    in Lambda (a, reduce env' false b)
  | x -> x

and reduce_appl (env : env) (force : bool) (f : expr) (x : expr) : expr =
  let reducedF = reduce env force f in
  let reducedX = reduce env force x in
  match reducedF with
  | Lambda (param, body) ->
    let env' = ((param, reducedX) :: env)
    in reduce_lambda env' force (reduce env' force body)
  | f' -> if force then raise (CannotBeApplied f') else Appl (f', reducedX)

and reduce_case (env : env) (force : bool) (x : expr) (y : int) (a : expr) (b : expr) : expr =
  match reduce env force x with
  | Integer x' -> reduce env force (if x' = y then a else b)
  | Symbol x' -> Case (Symbol x', y, a, b)
  | _ -> reduce env force b

and reduce_is_nil (env : env) (force : bool) (x : expr) (a : expr) (b : expr) : expr =
  match reduce env force x with
  | Nil -> reduce env force a
  | Symbol x' -> IsNil (Symbol x', a, b)
  | _ -> reduce env force b

and reduce_uncons (env : env) (force : bool) (x : expr) (f : expr) (a : expr) : expr =
  match reduce env force x with
  | Cons (m, n) -> reduce env force (Appl (Appl (f, m), n))
  | Symbol x' -> Uncons (Symbol x', f, a)
  | _ -> reduce env force a

and reduce (env : env) (force : bool) : expr -> expr = function
  | Symbol x -> reduce_symbol env force x
  | Appl (f, x) -> reduce_appl env force f x
  | Case (x, y, a, b) -> reduce_case env force x y a b
  | IsNil (x, a, b) -> reduce_is_nil env force x a b
  | Cons (a, b) -> Cons (reduce env force a, reduce env force b)
  | Uncons (x, f, a) -> reduce_uncons env force x f a
  | Eval (Quote x) -> reduce env force x
  | Eval x -> reduce env force x
  | x -> x (* Integers, nil and quotes cannot be reduced *)

let eval (env : env) : toplvl -> env*(expr option) =
  function
  | Define (var, value) -> ((var, reduce env true value) :: env, None)
  | Expr e -> (env, Some (reduce env true e))
  | Blank -> (env, None)
