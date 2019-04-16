open Syntax

let preludeTypes : gamma =
  [ (Var "()", TVar "()")
  ; (Var "id", Func (TVar "()", TVar "()"))
  ]

let preludeEnv : Eval.env =
  [ ("id", Lam ("x", Var "x"))
  ]

let program : expr = App (Var "id", Var "()")

let (Some (TVar "()")) = synthesize preludeTypes program
let (Var "()") = Eval.reduce preludeEnv program
