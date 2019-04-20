open Syntax

let preludeTypes : gamma =
  [
  ]

let preludeEnv : Eval.env =
  [
  ]

let prelude = (preludeTypes, preludeEnv)

let read s = Parser.sxp Lexer.token (Lexing.from_string s)

let print s =
  match s with
  | Eval.Ok e -> Syntax.show e
  | Eval.TypecheckFailed expr ->
    Printf.sprintf "Type checking failed: %s" (Syntax.showExpr expr)

let repr str = print (Eval.eval prelude (read str))

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
      in print_endline resp;
      (* i := i' *)
    done
  with End_of_file -> ()

let () = repl ()
