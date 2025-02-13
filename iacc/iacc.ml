open Syntax
open Assembly
open Writer

let emit_integer (x: int): unit asm64_writer =
  tell (Op_movl (LitInt x, Reg_eax))

(* TODO thread stack index *)
let rec emit_primcall: primcall -> unit asm64_writer =
  function
  | Add (x, y) ->
    emit_expr x *> (* TODO store on stack *)
    emit_expr y; (* TODO add with stack value *)
    (* TODO Printf.printf "addl $%i, %%eax\n" y *)
  | Sub (x, y) ->
    emit_expr x *> (* TODO store on stack *)
    emit_expr y; (* TODO add with stack value *)
    (* TODO Printf.printf "subl $%i, %%eax\n" y *)

and emit_expr: expr -> unit asm64_writer =
  function
  | PrimCall x ->
    emit_primcall x
  | Integer x -> emit_integer x

let compile_program (x: expr) : unit asm64_writer =
  emit_expr x *> tell Op_ret

let () = emit_all_asm (compile_program (PrimCall (Add (Integer 42, Integer 21))))
