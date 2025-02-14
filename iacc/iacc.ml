let compile_program (filename : string) : unit =
  Frontend.lex_and_parse filename
  |> Intermediate.expr_to_intermediate
  |> Backend.intermediate_to_asm64
  |> Writer.emit_all_asm

let () =
  compile_program "example"
