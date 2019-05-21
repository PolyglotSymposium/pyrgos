open Util
open Syntax
open Types

type env = (symbol * expr) list

exception TypeCheckingFailed of (expr*texpr)
exception TypeSynthesisFailed of expr
exception UpFailed of (expr * texpr)

let rec applyPieces env (arg : expr) (pieces : (expr*expr) list) : expr =
  List.fold_right (fun (a, b) acc ->
    match a with
    | Symbol param -> apply env param arg b
    | _ -> if a = arg then b else acc
  ) pieces (Lambda pieces)

and apply (env : env) (param : symbol) (arg : expr) (body : expr) : expr =
  (* TODO actually implement pattern matching *)
  let env' = if param = "_" || param.[0] = ':'
             then env (* don't bind constructors and _ *)
             else (param, arg) :: env
  in reduce env' body

and reduce (env : env) : expr -> expr = function
  | Appl (f, x) ->
    let reducedF = reduce env f in
    let reducedX = reduce env x in
    (match reducedF with
    | Lambda [Symbol arg, body] -> apply env arg reducedX body
    | Lambda pieces -> applyPieces env reducedX pieces
    | f' -> Appl (f', reducedX))
  | Lambda pieces ->
    Lambda (List.map (fun (a, b) -> (a, reduce env b)) pieces)
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
    let d = mkData (typ, ctrs)
    in UppedTheAnte (registerDataType d gamma, env)
  | e -> raise (UpFailed e)

let compilerEval (env : gamma*env) : toplvl -> result =
  function
  | Up expr -> up env expr
  | Expr expr -> Evaluated (eval env expr)

