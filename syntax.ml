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
  | DataCtr of symbol*ty

let unit : expr = DataCtr ("()", TVar "Unit")

let (>>) g f x = f(g(x))

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
  | DataCtr (x, _) -> x
  | Symbol x -> x

let show ((e, t) : expr*ty) : string =
  let e' = match e with | Annotate (e', _) -> e' | _ -> e
  in showExpr (Annotate (e', t))
