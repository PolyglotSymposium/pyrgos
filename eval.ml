open Syntax

type env = (symbol * expr) list

exception TypeCheckingFailed of (expr*ty)
exception TypeSynthesisFailed of expr
exception UpFailed of (expr * ty)

let rec reduce (env : env) : expr -> expr = function
  | Appl (f, x) ->
    (match reduce env f with
    | Lambda (arg, body) -> reduce ((arg, reduce env x) :: env) body
    | f' -> Appl (f', reduce env x))
  | Lambda (a, b) -> Lambda (a, reduce env b)
  | Symbol x ->
    (match List.find_all (fst >> (=) x) env with
    | [(_, t)] -> t
    | _ -> Symbol x)
  (* Quotes, atoms, and unit cannot be reduced *)
  | x -> x

let eval ((gamma, env) : Types.gamma*env) (e : expr) : (expr * ty) =
  (* TODO: OCaml Option.map? Or Functor instance? *)
  match Types.synthesize gamma e with
  | Some typ -> (reduce env e, typ)
  | None -> raise (TypeSynthesisFailed e)

type result =
  | Evaluated of (expr * ty)
  | UppedTheAnte of (Types.gamma * env)

let compilerEval ((gamma, env) as env' : Types.gamma*env) : toplvl -> result =
  function
  | Up expr ->
    (match eval env' expr with
    | (Quote e, _) -> Evaluated (eval env' e)
    | (Appl ((Appl (Appl (Symbol "let", Atom n), TExpr t)), Quote v), _) ->
      if Types.check gamma v t
      then UppedTheAnte ((Symbol n, t) :: gamma, (n, v) :: env)
      else raise (TypeCheckingFailed (v, t))
    | e -> raise (UpFailed e))
  | Expr expr -> Evaluated (eval env' expr)

