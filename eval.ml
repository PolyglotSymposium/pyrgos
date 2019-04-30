open Syntax

type env = (symbol * expr) list

exception TypeCheckingFailed of (expr*ty)
exception TypeSynthesisFailed of expr
exception UpFailed of (expr * ty)

let rec reduce (env : env) : expr -> expr = function
  | Appl (f, x) ->
    let reducedF = reduce env f in
    let reducedX = reduce env x in
    (match reducedF with
    | Lambda (arg, body) -> reduce ((arg, reducedX) :: env) body
    | f' -> Appl (f', reducedX))
  | Lambda (a, b) -> Lambda (a, reduce env b)
  | Symbol x ->
    (match List.find_all (fst >> (=) x) env with
    | [(_, e)] -> e
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

let up ((gamma, env) as env' : Types.gamma*env) (expr : expr) : result =
  match eval env' expr with
  | (Quote e, _) -> Evaluated (eval env' e)
  | (Appl ((Appl (Appl (Symbol "let", Atom n), TExpr t)), Quote v), _) ->
    if Types.check gamma v t
    then
      let v = reduce env v
      in UppedTheAnte ((Symbol n, t) :: gamma, (n, v) :: env)
    else raise (TypeCheckingFailed (v, t))
  | ((Appl (Appl (Symbol "data", Atom typ), List ctrs)), _) ->
    (* TODO what safety checks are needed here? *)
    let ctrs' = List.map (fun x -> (Symbol x, TVar typ)) ctrs
    in UppedTheAnte (ctrs' @ gamma, env)
  | e -> raise (UpFailed e)

let compilerEval (env : Types.gamma*env) : toplvl -> result =
  function
  | Up expr -> up env expr
  | Expr expr -> Evaluated (eval env expr)

