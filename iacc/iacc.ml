open Syntax
open Assembly
open Writer

let integer (x: int64): unit asm64_writer =
  tell (Op_mov (LitInt64 x, Reg_rax))

let push (reg : register) : unit asm64_writer =
  tell (Op_push (Register reg))

let pop (reg : register) : unit asm64_writer =
  tell (Op_pop reg)

let add (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_add (Register reg1, reg2))

let sub (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_sub (Register reg1, reg2))

let move (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_mov (Register reg1, reg2))

let rec primcall: primcall -> unit asm64_writer =
  function
  | Add (x, y) ->
    expr x *>
    push Reg_rax *>
    expr y *>
    pop Reg_rbx *>
    add Reg_rbx Reg_rax
  | Sub (x, y) ->
    expr x *>
    push Reg_rax *>
    expr y *>
    move Reg_rax Reg_rbx *>
    pop Reg_rax *>
    sub Reg_rbx Reg_rax

and expr: expr -> unit asm64_writer =
  function
  | PrimCall x ->
    primcall x
  | Integer x -> integer x

let compile_program (x: expr) : unit asm64_writer =
  expr x *> tell Op_ret

let () = emit_all_asm (compile_program (PrimCall (Sub (Integer 42L, Integer 16L))))
