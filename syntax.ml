type symbol = string (* TODO make this right *)

type ty =
  | TVar of symbol
  | Func of ty * ty

type expr =
  | Annotate of expr*ty
  | Appl of expr * expr
  | Atom of symbol
  | Lambda of symbol * expr
  | Quote of expr
  | Symbol of symbol
  | Unit

type gamma = (expr*ty) list

let (>>) g f x = f(g(x))

let rec check (g : gamma) (e : expr) (t : ty) : bool =
  match e with
  | Lambda (arg, body) ->
    (match t with
    | Func (t1, t2) -> check ((Symbol arg, t1) :: g) body t2
    | _ -> false (* We have no type aliasiang mechanism *))
  | _ -> synthesize g e = Some t

and synthesize (g : gamma) : expr -> ty option = function
  | Annotate (e, t) -> if check g e t then Some t else None
  | Appl (f, x) ->
    (match synthesize g f with
    | Some (Func (t1, t2)) -> if check g x t1 then Some t2 else None
    | _ -> None)
  | Atom _ -> Some (TVar "Atom") (* The Atom type is an infinite disjunction. *)
  | Lambda _ -> None (* There is no synthesis rule for lambdas *)
  | Quote e -> Some (TVar "S-Expr")
  | Unit -> Some (TVar "Unit")
  | Symbol v ->
    (match List.find_all (fst >> (=) (Symbol v)) g with
    | [(_, t)] -> Some t
    | _ -> None)

let rec showType : ty -> string = function
  | TVar name -> name
  | Func (TVar i, o) -> Printf.sprintf "%s -> %s" i (showType o)
  | Func (i, o) -> Printf.sprintf "(%s) -> %s" (showType i) (showType o)

let rec showExpr : expr -> string = function
  | Annotate (e, t) -> Printf.sprintf "(%s : %s)" (showExpr e) (showType t)
  | Appl (f, x) -> Printf.sprintf "(%s %s)" (showExpr f) (showExpr x)
  | Atom x -> Printf.sprintf "#%s" x
  | Lambda (a, b) -> Printf.sprintf "(%s => %s)" a (showExpr b)
  | Quote e -> Printf.sprintf "'%s" (showExpr e)
  | Unit -> "()"
  | Symbol x -> x

let show ((e, t) : expr*ty) : string =
  let e' = match e with | Annotate (e', _) -> e' | _ -> e
  in showExpr (Annotate (e', t))
