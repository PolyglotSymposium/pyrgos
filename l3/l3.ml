open Util
open Syntax

let read s = Parser.toplvl Lexer.token (Lexing.from_string s)

let repr (env : Eval.env) (str : string) : Eval.env*(string option) =
  let (env', maybeExpr) = Eval.eval env (read str)
  in (env', map_opt Syntax.show maybeExpr)

let repl () =
  try
    let envRef = ref [] in
    while true do
      print_string "l3> ";
      let resp =
        try
          let env, resp = repr !envRef (read_line ())
          in envRef := env; resp
        with
        | Eval.UnboundVariable x ->
          Some (Printf.sprintf "Unbound variable %s" x)
        | Eval.CannotBeApplied x ->
          Some (Printf.sprintf "Cannot apply non-function: %s" (Syntax.show x))
        | Parser.Error -> Some "Parser error."
        | Failure "lexing: empty token" -> Some "Ignored."
      in match resp with
         | Some resp' -> print_endline resp'
         | None -> ()
    done
  with End_of_file -> ()

let () = repl ()
