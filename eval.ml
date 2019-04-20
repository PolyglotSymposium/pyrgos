open Syntax

type env = (Syntax.symbol * expr) list

type result =
  | Ok of (expr * ty)
  | TypecheckFailed of expr

let rec reduce (env : env) : expr -> expr = function
  | Annotate (e, _) -> reduce env e
  | Appl (f, x) ->
    let (Lambda (arg, body)) = reduce env f
    in reduce ((arg, reduce env x) :: env) body
  | Lambda (a, b) -> Lambda (a, reduce env b)
  | Symbol x ->
    (match List.find_all (fst >> (=) x) env with
    | [(_, t)] -> t
    | _ -> Symbol x)
  | x -> x

let eval ((gamma, env) : gamma*env) (e : expr) : result =
  (* TODO: OCaml Option.map? Or Functor instance? *)
  match synthesize gamma e with
  | Some typ -> let reduced = reduce env e in Ok (reduced, typ)
  | None -> TypecheckFailed e
