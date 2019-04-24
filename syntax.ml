type symbol = string (* TODO make this right *)

type ty =
  | TVar of symbol
  | Func of ty * ty

(* Lifted value, as it were, encoded as a void type *)
type allowsEval = { __ : 'a . 'a }

type 'a expr =
  | Annotate : 'a expr*ty -> 'a expr
  | Appl : 'a expr * 'a expr -> 'a expr
  | Atom : symbol -> 'a expr
  | Eval : allowsEval expr -> allowsEval expr
  | Lambda : symbol * 'a expr -> 'a expr
  | Quote : 'a expr -> 'a expr
  | Symbol : symbol -> 'a expr

type 'a toplvl =
  | Up of 'a expr
  | Expr of 'a expr

let (>>) g f x = f(g(x))

let rec showType : ty -> string = function
  | TVar name -> name
  | Func (TVar i, o) -> Printf.sprintf "%s -> %s" i (showType o)
  | Func (i, o) -> Printf.sprintf "(%s) -> %s" (showType i) (showType o)

let rec showExpr : unit expr -> string = function
  | Annotate (e, t) -> Printf.sprintf "(%s : %s)" (showExpr e) (showType t)
  | Appl (f, x) -> Printf.sprintf "(%s %s)" (showExpr f) (showExpr x)
  | Atom x -> Printf.sprintf "#%s" x
  | Lambda (a, b) -> Printf.sprintf "(%s => %s)" a (showExpr b)
  | Quote e -> Printf.sprintf "'%s" (showExpr e)
  | Symbol x -> x

let show ((e, t) : unit expr*ty) : string =
  let e' = match e with | Annotate (e', _) -> e' | _ -> e
  in showExpr (Annotate (e', t))
