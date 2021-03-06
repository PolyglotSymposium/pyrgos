open Util

type symbol = string (* TODO make this right *)

type texpr =
  | Func of texpr * texpr
  | TVar of symbol

type expr =
  | Appl of expr * expr
  | Atom of symbol
  | DataCtr of symbol
  | Lambda of (expr * expr) list
  | List of symbol list
  | Quote of expr
  | Symbol of symbol
  | TExpr of texpr

type toplvl =
  | Up of expr (* the $ operator *)
  | Expr of expr

let rec showType : texpr -> string = function
  | Func (TVar i, o) -> Printf.sprintf "%s -> %s" i (showType o)
  | Func (i, o) -> Printf.sprintf "(%s) -> %s" (showType i) (showType o)
  | TVar name -> name

(* TODO pretty-print *)
let rec showExpr : expr -> string = function
  | Appl (f, x) -> Printf.sprintf "(%s %s)" (showExpr f) (showExpr x)
  | Atom x -> Printf.sprintf "#%s" x
  | DataCtr x -> Printf.sprintf ":%s" x
  | Lambda pieces ->
    let showPiece (a, b) = Printf.sprintf "%s => %s" (showExpr a) (showExpr b) in
    let ps = List.map showPiece pieces in
    let lBody = String.concat " | " ps in
    Printf.sprintf "(%s)" lBody
  | List xs ->
    let lBody = unwords (List.map (fun x -> showExpr (Atom x)) xs)
    in Printf.sprintf "[%s]" lBody
  | Quote e -> Printf.sprintf "'%s" (showExpr e)
  | Symbol x -> x
  | TExpr t -> Printf.sprintf "{%s}" (showType t)

let show ((e, t) : expr*texpr) : string =
  Printf.sprintf "%s : %s" (showExpr e) (showType t)
