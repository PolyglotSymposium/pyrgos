open Syntax

let preludeTypes : gamma =
  [
  ]

let preludeEnv : Eval.env =
  [
  ]

let prelude = (preludeTypes, preludeEnv)

let read s = Parser.sxp Lexer.token (Lexing.from_string s)

let eval ((gamma, env) : gamma*Eval.env) (e : expr) : (expr*ty) option =
  (* TODO: OCaml Option.map? *)
  match synthesize gamma e with
  | Some typ -> let reduced = Eval.reduce env e in Some (reduced, typ)
  | None -> None

let print s =
  match s with
  | None -> "Type checking failed."
  | Some x -> Syntax.show x

let rep str = print (eval prelude (read str))

(* Originally derived from the OCaml implementation of Make a Lisp *)
let repl () =
  try
    let i = ref 0 in (* This is supposed to be the level of the tower *)
    while true do
      Printf.printf "%i> " !i;
      let resp =
        try
          try rep (read_line ())
          with Parsing.Parse_error -> "Parsing failed."
        with Parser.Error -> "Parser error."
      in print_endline resp;
      (* i := i' *)
    done
  with End_of_file -> ()

let () = repl ()
