let lex_and_parse (filename : string) : Syntax.expr =
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in
  (* Set filename for error reporting *)
  let () = Lexing.set_filename lexbuf filename in
  Parser.program Lexer.token lexbuf
