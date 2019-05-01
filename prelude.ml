open Syntax
open Types

let unit : symbol = "()"
let nil : expr = List []
let let' : expr = Symbol "let"
let enum : expr = Symbol "enum"

let tAtom : texpr = TVar "Atom"
let tData : texpr = TVar "Data"
let tExpr : texpr = TVar "Expr"
let tLet : texpr = TVar "Let"
let tList : texpr = TVar "List"
let tTypeExpr : texpr = TVar "Type-Expr"
let tUnit : symbol = "Unit"

let types : gamma =
  mkGamma
    [ (tUnit, [unit])
    ]
    [ (nil, tList)
    ; (let', Func (tAtom, Func (tTypeExpr, Func (tExpr, tLet))))
    ; (enum, Func (tAtom, Func (tList, tData)))
    ]

(* This is Eval.env, but because of Prelude being bootstrapped in, we can't
 * reference it that way without a circular dependency. *)
let env : (symbol * expr) list =
  [
  ]

let prelude : gamma*(symbol * expr) list = (types, env)
