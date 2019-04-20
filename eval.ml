open Syntax

type env = (symbol * expr) list

type result =
  | Ok of (expr * ty)
  | TypecheckFailed of expr

let rec reduce (env : env) : expr -> expr = function
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

let eval ((gamma, env) : Types.gamma*env) (e : expr) : result =
  (* TODO: OCaml Option.map? Or Functor instance? *)
  match Types.synthesize gamma e with
  | Some typ -> let reduced = reduce env e in Ok (reduced, typ)
  | None -> TypecheckFailed e
