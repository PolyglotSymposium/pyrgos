type symbol = string (* TODO make this right *)

type ty =
  | Func of ty * ty
  | TVar of symbol

type expr =
  | Appl of expr * expr
  | Atom of symbol
  | Lambda of symbol * expr
  | Quote of expr
  | Symbol of symbol
  | TExpr of ty

type toplvl =
  | Up of expr (* the $ operator *)
  | Expr of expr

let (>>) g f x = f(g(x))

let rec showType : ty -> string = function
  | Func (TVar i, o) -> Printf.sprintf "%s -> %s" i (showType o)
  | Func (i, o) -> Printf.sprintf "(%s) -> %s" (showType i) (showType o)
  | TVar name -> name

let rec showExpr : expr -> string = function
  | Appl (f, x) -> Printf.sprintf "(%s %s)" (showExpr f) (showExpr x)
  | Atom x -> Printf.sprintf "#%s" x
  | Lambda (a, b) -> Printf.sprintf "(%s => %s)" a (showExpr b)
  | Quote e -> Printf.sprintf "'%s" (showExpr e)
  | Symbol x -> x
  | TExpr t -> Printf.sprintf "{%s}" (showType t)

let show ((e, t) : expr*ty) : string =
  Printf.sprintf "%s : %s" (showExpr e) (showType t)
