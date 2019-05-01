open Syntax

type env = Types.gamma*Eval.env

let read s = Parser.toplvl Lexer.token (Lexing.from_string s)

let repr (env : env) (str : string) : env*string =
  match Eval.compilerEval env (read str) with
  | Eval.Evaluated expr -> (env, Syntax.show expr)
  | Eval.UppedTheAnte env -> (env, "Upped the ante.")

let repl () =
  print_endline "Welcome. Pyrgos at your service.";
  try
    let envRef = ref Prelude.prelude in
    while true do
      print_string "> ";
      let resp =
        try
          let env, resp = repr !envRef (read_line ())
          in envRef := env; resp
        with
        | Parser.Error -> "Parser error."
        | Failure "lexing: empty token" -> "Ignored."
        | Eval.TypeSynthesisFailed expr ->
          Printf.sprintf "Type synthesis failed: %s" (Syntax.showExpr expr)
        | Eval.TypeCheckingFailed e ->
          Printf.sprintf "Fails to type check: %s" (Syntax.show e)
        | Eval.UpFailed e -> Printf.sprintf "Up failed: %s" (Syntax.show e)
        | Types.DataCtrAlreadyDefined (ctr, newT, existingT) ->
          let n = Syntax.showType newT in
          let x = Syntax.showType existingT in
          Printf.sprintf "Cannot define data constructor %s for type %s: " ctr n
          ^ Printf.sprintf "already defined as a constructor of %s" x
        | Types.TypeAlreadyDefined t ->
          Printf.sprintf "Type already defined: %s" t
        | Types.CtrsMustStartWithColon ctr ->
          "Constructors must start with : to avoid ambiguity in pattern matches"
          ^ Printf.sprintf " but %s does not" ctr
      in print_endline resp
    done
  with End_of_file -> print_endline "\rAlways at your service. Goodbye."

let () = repl ()
