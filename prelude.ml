open Syntax

let unit : expr = Symbol "()"
let nil : expr = List []
let let' : expr = Symbol "let"
let data : expr = Symbol "data"

let tUnit : ty = TVar "Unit"
let tAtom : ty = TVar "Atom"
let tList : ty = TVar "List"
let tExpr : ty = TVar "Expr"
let tTypeExpr : ty = TVar "Type-Expr"
let tToUp : ty = TVar "To-Up"

(* This is Types.gamma, but because of Prelude being bootstrapped in, we can't
 * reference it that way without a circular dependency. *)
let types : (expr*ty) list =
  [ (unit, tUnit)
  ; (nil, tList)
  ; (let', Func (tAtom, Func (tTypeExpr, Func (tExpr, tToUp))))
  ; (data, Func (tAtom, Func (tList, tToUp)))
  ]

(* This is Eval.env, but because of Prelude being bootstrapped in, we can't
 * reference it that way without a circular dependency. *)
let env : (symbol * expr) list =
  [
  ]

let prelude : (expr*ty) list*(symbol * expr) list = (types, env)
