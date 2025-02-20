open Base
open Base.Int64
open Intermediate
open Assembly
open Writer

let label_counter = ref 0L

let unique_label () : label =
  let x = !label_counter in
  label_counter := x + 1L;
  Lbl (Printf.sprintf "lbl%Ld" x)

let integer (x: int64): unit asm64_writer =
  tell (Op_mov (LitInt64 x, Dst_register Reg_rax))

let push (reg : register) : unit asm64_writer =
  tell (Op_push (Src_register reg))

let pop (reg : register) : unit asm64_writer =
  tell (Op_pop reg)

let drop (n : int64) : unit asm64_writer =
  tell (Op_lea (Src_offset (n * 8L, Reg_rsp), Reg_rsp))

let add (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_add (Src_register reg1, Dst_register reg2))

let sub (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_sub (Src_register reg1, Dst_register reg2))

let move_reg (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_mov (Src_register reg1, Dst_register reg2))

let move_offset (offset: int64) (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_mov (Src_offset (offset,  reg1), Dst_register reg2))

let heap_pointer = Reg_esi

let allocate (size : int64) : unit asm64_writer =
  tell (Op_mov (Src_register heap_pointer, Dst_register Reg_rax)) *>
  tell (Op_add (LitInt64 size, Dst_register heap_pointer))

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
    tell (Op_cmp (Src_register Reg_rbx, Src_register Reg_rax))

and intermediate_to_asm64_ (non_let: int64) : intermediate -> unit asm64_writer =
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
    move_offset ((non_let + index) * 8L) Reg_rsp Reg_rax
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
  | IR_MkArray args ->
    let size = Int64.of_int (List.length args) * 8L in (* each element is 8 bytes/64 bits *)
    let _ = allocate size (* TODO *> *)
    in failwith "TODO: MkArray implementation incomplete"
    (* Store each argument in the array *)
    (* TODO List.foldi args writer_unit (fun addr arg_writer arg ->
      arg_writer *>
      tell (Op_mov (Src_register Reg_rax, Dst_offset (Int64.of_int addr * 8L, Reg_rax))) (* Store arg in array *)
    ) *)
    (* TODO return array pointer *)
  | IR_ArrayIndex (index, array) ->
    intermediate_to_asm64_ non_let array *>
    move_offset (Int64.of_int index * 8L) Reg_rax Reg_rax

let intermediate_to_asm64 (e: intermediate) : unit asm64_writer =
  intermediate_to_asm64_ 0L e *>
  tell Op_ret
