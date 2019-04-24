open Syntax

type env = (symbol * unit expr) list

type result = unit expr * ty

exception TypecheckFailed of unit expr
exception UpFailed of (unit expr * ty)
exception RunEvalsFailed of (unit expr * ty)

let rec reduce (env : env) : unit expr -> unit expr = function
  | Annotate (e, _) -> reduce env e
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

let eval ((gamma, env) : Types.gamma*env) (e : unit expr) : result =
  (* TODO: OCaml Option.map? Or Functor instance? *)
  match Types.synthesize gamma e with
  | Some typ -> (reduce env e, typ)
  | None -> raise (TypecheckFailed e)

let rec runEvals (env : Types.gamma*env) : allowsEval expr -> unit expr =
  function
  | Annotate (e, t) -> Annotate (runEvals env e, t)
  | Appl (e1, e2) -> Appl (runEvals env e1, runEvals env e2)
  | Atom s -> Atom s
  | Eval e ->
    (match eval env (runEvals env e) with
    | (Quote e, TVar "Expr") -> e
    | (e, t) -> raise (RunEvalsFailed (e, t)))
  | Lambda (a, b) -> Lambda (a, runEvals env b)
  | Quote e -> Quote (runEvals env e)
  | Symbol s -> Symbol s

let eval' (env : Types.gamma*env) (e : allowsEval expr) : result =
  eval env (runEvals env e)

let compilerEval (env : Types.gamma*env) : allowsEval toplvl -> result =
  function
  | Up expr ->
    let e = eval' env expr
    in raise (UpFailed e) (* TODO: implement up and tower levels *)
  | Expr expr -> eval' env expr

