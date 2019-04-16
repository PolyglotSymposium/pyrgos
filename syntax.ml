type name = string

type ty =
  | TVar of name
  | Func of ty * ty

type expr =
  | Var of name
  | Lam of name * expr
  | App of expr * expr

type gamma = (expr*ty) list

let todo x = failwith "TODO"
let (>>) g f x = f(g(x))

let rec check (g : gamma) (e : expr) (t : ty) : bool =
  match e with
  | Lam (arg, body) ->
    (match t with
    | Func (t1, t2) -> check ((Var arg, t1) :: g) body t2
    | _ -> false (* We have no type aliasiang mechanism *))
  | _ -> synthesize g e = Some t

and synthesize (g : gamma) : expr -> ty option = function
  | Var v ->
    (match List.find_all (fst >> (=) (Var v)) g with
    | [(_, t)] -> Some t
    | _ -> None)
  | Lam _ -> None (* There is no synthesis rule for lambdas *)
  | App (f, x) ->
    match synthesize g f with
    | Some (Func (t1, t2)) -> if check g x t1 then Some t2 else None
    | _ -> None
