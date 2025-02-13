open Syntax
open Assembly
open Writer

let integer (x: int64): unit asm64_writer =
  tell (Op_mov (LitInt64 x, Reg_rax))

let push (reg : register) : unit asm64_writer =
  tell (Op_push (Register reg))

let pop (reg : register) : unit asm64_writer =
  tell (Op_pop reg)

let drop (n : int64) : unit asm64_writer =
  tell (Op_lea (Offset (Int64.mul n 8L, Reg_rsp), Reg_rsp))

let add (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_add (Register reg1, reg2))

let sub (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_sub (Register reg1, reg2))

let move_reg (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_mov (Register reg1, reg2))

let move_offset (offset: int64) (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_mov (Offset (offset,  reg1), reg2))

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
    move_reg Reg_rax Reg_rbx *>
    pop Reg_rax *>
    sub Reg_rbx Reg_rax

and expr: expr -> unit asm64_writer =
  function
  | PrimCall x ->
    primcall x
  | Integer x -> integer x
  | Let (_name, value, body) ->
    expr value *>
    push Reg_rax *>
    expr body *>
    drop 1L
  | Variable _name ->
    let offset =  (* Calculate the offset of the variable on the stack *)
      (* TODO: Keep track of variable offsets in a stack/environment *)
      (* Placeholder: For now, assume all variables are at the top of the stack. *)
      0L (* Replace with actual offset *)
    in move_offset offset Reg_rsp Reg_rax

let compile_program (x: expr) : unit asm64_writer =
  expr x *> tell Op_ret

let () =
  let example = Let ("x", PrimCall (Add (Integer 36L, Integer 22L)), PrimCall (Sub (Variable "x", Integer 16L)))
  in emit_all_asm (compile_program example)
