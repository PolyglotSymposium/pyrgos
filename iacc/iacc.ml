let compile_program (filename : string) : unit =
  let asm =
    Frontend.lex_and_parse filename
    |> Intermediate.expr_to_intermediate
    |> Backend.intermediate_to_asm64
  and out = open_out (filename ^ ".s") in
  Writer.emit_all_asm out asm;
  close_out out

let () =
  compile_program "testbed/example"
