open Syntax
open Intermediate1
open Intermediate2

let write_expr (filename : string) (e : expr) : unit =
  let out = open_out (filename ^ ".expr.el") in
  Sexplib.Sexp.output_hum out (sexp_of_expr e);
  close_out out

let write_ir1 (filename : string) (ir1 : intermediate1) : unit =
  let out = open_out (filename ^ ".ir1.el") in
  Sexplib.Sexp.output_hum out (sexp_of_intermediate1 ir1);
  close_out out

let write_ir2 (filename : string) (ir2 : intermediate2) : unit =
  let out = open_out (filename ^ ".ir2.el") in
  Sexplib.Sexp.output_hum out (sexp_of_intermediate2 ir2);
  close_out out

let write_asm (filename : string) asm : unit =
  let out = open_out (filename ^ ".s") in
  Writer.emit_all_asm out asm;
  close_out out

let compile_program (filename : string) : unit =
  let syntax = Frontend.lex_and_parse filename in
  let ir1 = Stage1.expr_to_ir1 syntax in
  let ir2 = Stage2.ir1_to_2 ir1 in
  let asm = Backend.intermediate_to_asm64 ir2 in
  write_expr filename syntax;
  write_ir1 filename ir1;
  write_ir2 filename ir2;
  write_asm filename asm

let () =
  compile_program "testbed/example"
