open Syntax

let unit : expr = Symbol "()"
let nil : expr = Symbol "[]"
let cons : expr = Symbol "::"
let consCtr (e1 : expr) (e2 : expr) : expr = Appl (Appl (cons, e1), e2)

let tUnit : ty = TVar "Unit"
let tAtom : ty = TVar "Atom"
let tList : ty = TVar "List"
let tExpr : ty = TVar "Expr"
let tTypeExpr : ty = TVar "Type-Expr"

(* This is Types.gamma, but because of Prelude being bootstrapped in, we can't
 * reference it that way without a circular dependency. *)
let types : (expr*ty) list =
  [ (unit, tUnit)
  ; (nil, tList)
  ; (cons, Func (tAtom, Func (tList, tList)))
  ]

(* This is Eval.env, but because of Prelude being bootstrapped in, we can't
 * reference it that way without a circular dependency. *)
let env : (symbol * expr) list =
  [
  ]

let prelude : (expr*ty) list*(symbol * expr) list = (types, env)
