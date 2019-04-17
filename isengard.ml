open Syntax

let preludeTypes : gamma =
  [ (Var "()", TVar "()")
  ; (Var "id", Func (TVar "()", TVar "()"))
  ]

let preludeEnv : Eval.env =
  [ ("id", Lam ("x", Var "x"))
  ]

let prelude = (preludeTypes, preludeEnv)

let run ((gamma, env) : gamma*Eval.env) (e : expr) : string =
  match synthesize gamma e with
  | Some typ ->
    let reduced = Eval.reduce env e
    in show (reduced, typ)
  | None -> "Does not type-check."

let program : expr = App (Var "id", Var "()")

let () = print_endline (run prelude program)
