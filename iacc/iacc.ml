open Syntax
open Intermediate

let write_expr (filename : string) (e : expr) : unit =
  let out = open_out (filename ^ ".expr.el") in
  Sexplib.Sexp.output_hum out (sexp_of_expr e);
  close_out out

let write_ir (filename : string) (ir : intermediate) : unit =
  let out = open_out (filename ^ ".ir.el") in
  Sexplib.Sexp.output_hum out (sexp_of_intermediate ir);
  close_out out

let write_asm (filename : string) asm : unit =
  let out = open_out (filename ^ ".s") in
  Writer.emit_all_asm out asm;
  close_out out

let compile_program (filename : string) : unit =
  let syntax = Frontend.lex_and_parse filename in
  let ir = Middle.expr_to_intermediate syntax in
  let asm = Backend.intermediate_to_asm64 ir in
  write_expr filename syntax;
  write_ir filename ir;
  write_asm filename asm

let () =
  compile_program "testbed/example"
