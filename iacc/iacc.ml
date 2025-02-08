open Syntax

let emit_integer (x: int): unit =
  Printf.printf "movl $%i, %%eax\n" x

let emit_primcall: primcall -> unit =
  function
  | Add (x, y) ->
    emit_integer x;
    Printf.printf "addl $%i, %%eax\n" y
  | Sub (x, y) ->
    emit_integer x;
    Printf.printf "subl $%i, %%eax\n" y

let emit_expr: expr -> unit =
  function
  | PrimCall x ->
    emit_primcall x
  | Integer x -> emit_integer x

let compile_program (x: expr) : unit =
  emit_expr x;
  print_endline "ret"

let () = compile_program (PrimCall (Add (42, 21)))
