open Syntax

let preludeTypes : Types.gamma =
  [ ("()", TVar "Unit")
  ]

let preludeEnv : Eval.env =
  [
  ]

let prelude = (preludeTypes, preludeEnv)

let read s = Parser.toplvl Lexer.token (Lexing.from_string s)

let repr str = Syntax.show (Eval.compilerEval prelude (read str))

(* Originally derived from the OCaml implementation of Make a Lisp *)
let repl () =
  try
    let i = ref 0 in (* This is supposed to be the level of the tower *)
    while true do
      Printf.printf "%i> " !i;
      let resp =
        try repr (read_line ())
        with
        | Parser.Error -> "Parser error."
        | Failure "lexing: empty token" -> "Ignored."
        | Eval.TypecheckFailed expr ->
          Printf.sprintf "Type checking failed: %s" (Syntax.showExpr expr)
        | Eval.UpFailed e -> Printf.sprintf "Up failed: %s" (Syntax.show e)
        | Eval.RunEvalsFailed e ->
          Printf.sprintf "Running ! evals failed: expression does not have type Expr: %s" (Syntax.show e)
      in print_endline resp;
      (* i := i' *)
    done
  with End_of_file -> ()

let () = repl ()
