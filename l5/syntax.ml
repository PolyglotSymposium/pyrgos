open Util

type symbol = string (* TODO make this right *)

type expr =
  | Symbol of symbol
  | Lambda of symbol * expr
  | Appl of expr * expr
  | Integer of int
  | Case of expr * int * expr * expr
  | Nil
  | IsNil of expr * expr * expr
  | Cons of expr * expr
  | Uncons of expr * expr * expr
  | Quote of expr
  | Eval of expr

type toplvl =
  | Define of symbol * expr
  | Expr of expr
  | Blank

let rec show : expr -> string = function
  | Symbol x -> x
  | Lambda (a, b) -> Printf.sprintf "{$\\ %s %s}" a (show b)
  | Appl (f, x) -> Printf.sprintf "{$$ %s %s}" (show f) (show x)
  | Integer x -> Printf.sprintf "%i" x
  | Case (x, y, a, b) ->
    Printf.sprintf "{$= %s %i %s %s}" (show x) y (show a) (show b)
  | Nil -> "{$_}"
  | IsNil (x, a, b) ->
    Printf.sprintf "{$- %s %s %s}" (show x) (show a) (show b)
  | Cons (a, b) ->
    Printf.sprintf "{$, %s %s}" (show a) (show b)
  | Uncons (x, f, a) ->
    Printf.sprintf "{$* %s %s %s}" (show x) (show f) (show a)
  | Quote q -> Printf.sprintf "{$' %s}" (show q)
  | Eval e -> Printf.sprintf "{$> %s}" (show e)

