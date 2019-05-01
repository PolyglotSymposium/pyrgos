open Syntax
open Types

type env = (symbol * expr) list

exception TypeCheckingFailed of (expr*texpr)
exception TypeSynthesisFailed of expr
exception UpFailed of (expr * texpr)

let rec reduce (env : env) : expr -> expr = function
  | Appl (f, x) ->
    let reducedF = reduce env f in
    let reducedX = reduce env x in
    (match reducedF with
    | Lambda [arg, body] -> reduce ((arg, reducedX) :: env) body
    | Lambda _ -> failwith "NOT IMPLEMENTED YET"
    | f' -> Appl (f', reducedX))
  | Lambda [a, b] -> Lambda [a, reduce env b]
  | Lambda _ -> failwith "NOT IMPLEMENTED YET"
  | Symbol x ->
    (match List.find_all (fst >> (=) x) env with
    | [(_, e)] -> e
    | _ -> Symbol x)
  (* Quotes, atoms, and unit cannot be reduced *)
  | x -> x

let eval ((gamma, env) : gamma*env) (e : expr) : (expr * texpr) =
  (* TODO: OCaml Option.map? Or Functor instance? *)
  match Checking.synthesize gamma e with
  | Some typ -> (reduce env e, typ)
  | None -> raise (TypeSynthesisFailed e)

type result =
  | Evaluated of (expr * texpr)
  | UppedTheAnte of (gamma * env)

let up ((gamma, env) as env' : gamma*env) (expr : expr) : result =
  match eval env' expr with
  | (Quote e, _) -> Evaluated (eval env' e)
  | (Appl ((Appl (Appl (Symbol "let", Atom n), TExpr t)), Quote v), _) ->
    if Checking.check gamma v t
    then
      let v = reduce env v
      in UppedTheAnte (registerExprType (Symbol n) t gamma, (n, v) :: env)
    else raise (TypeCheckingFailed (v, t))
  | ((Appl (Appl (Symbol "enum", Atom typ), List ctrs)), _) ->
    let d = { name = typ; ctrs = ctrs }
    in UppedTheAnte (registerDataType d gamma, env)
  | e -> raise (UpFailed e)

let compilerEval (env : gamma*env) : toplvl -> result =
  function
  | Up expr -> up env expr
  | Expr expr -> Evaluated (eval env expr)

