open Intermediate
open Assembly
open Writer

let label_counter = ref 0L

let unique_label () : label =
  let x = !label_counter in
  label_counter := Int64.add x 1L;
  Lbl (Printf.sprintf "lbl%Ld" x)

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

let rec primcall (non_let: int64): primcall -> unit asm64_writer =
  function
  | IR_Add (x, y) ->
    intermediate_to_asm64_ non_let x *>
    push Reg_rax *>
    intermediate_to_asm64_ (Int64.succ non_let) y *>
    pop Reg_rbx *>
    add Reg_rbx Reg_rax
  | IR_Sub (x, y) ->
    intermediate_to_asm64_ non_let x *>
    push Reg_rax *>
    intermediate_to_asm64_ (Int64.succ non_let) y *>
    move_reg Reg_rax Reg_rbx *>
    pop Reg_rax *>
    sub Reg_rbx Reg_rax
  | IR_Eq (x, y) ->
    intermediate_to_asm64_ non_let x *>
    push Reg_rax *>
    intermediate_to_asm64_ (Int64.succ non_let) y *>
    pop Reg_rbx *>
    tell (Op_cmp (Register Reg_rbx, Register Reg_rax))

and intermediate_to_asm64_ (non_let: int64): intermediate -> unit asm64_writer =
  function
  | IR_PrimCall x ->
    primcall non_let x
  | IR_Integer x ->
    integer x
  | IR_Let (value, body) ->
    intermediate_to_asm64_ non_let value *>
    push Reg_rax *>
    intermediate_to_asm64_ non_let body *>
    drop 1L
  | IR_Variable index ->
    move_offset (Int64.mul (Int64.add non_let index) 8L) Reg_rsp Reg_rax
  | IR_IfThenElse ite ->
    intermediate_to_asm64_ non_let ite.condition *>
    let else_label = unique_label () in
    let end_label = unique_label () in
    tell (Op_je else_label) *>
    intermediate_to_asm64_ non_let ite.branch2 *>
    tell (Op_jmp end_label) *>
    tell (Label else_label) *>
    intermediate_to_asm64_ non_let ite.branch1 *>
    tell (Label end_label)
  | IR_Fail _ ->
    failwith "IR exceptions are not implemented (yet)"
  | IR_MkArray (_args) ->
    failwith "IR tuples are not fully implemented (yet)"
  | IR_ArrayIndex (_length, _array) ->
    failwith "IR tuples are not fully implemented (yet)"

let intermediate_to_asm64 (e: intermediate) : unit asm64_writer =
  intermediate_to_asm64_ 0L e *>
  tell Op_ret
