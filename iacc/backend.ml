open Base
open Base.Int64
open Intermediate2
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

let and_ (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_and (Src_register reg1, Dst_register reg2))

let sub (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_sub (Src_register reg1, Dst_register reg2))

let move_reg (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_mov (Src_register reg1, Dst_register reg2))

let move_from_offset (offset: int64) (reg1 : register) (reg2 : register) : unit asm64_writer =
  tell (Op_mov (Src_offset (offset,  reg1), Dst_register reg2))

let move_to_offset (reg1 : register) (offset: int64) (reg2 : register) : unit asm64_writer =
  tell (Op_mov (Src_register reg1, Dst_offset (offset, reg2)))

let heap_pointer = Reg_rsi

let allocate (size : int64) : unit asm64_writer =
  tell (Op_mov (Src_register heap_pointer, Dst_register Reg_rax)) *>
  tell (Op_add (LitInt64 size, Dst_register heap_pointer))

let rec primcall (non_let: int64): primcall -> unit asm64_writer =
  function
  | IR2_Add (x, y) ->
    intermediate_to_asm64_ non_let x *>
    push Reg_rax *>
    intermediate_to_asm64_ (Int64.succ non_let) y *>
    pop Reg_rbx *>
    add Reg_rbx Reg_rax
  | IR2_Sub (x, y) ->
    intermediate_to_asm64_ non_let x *>
    push Reg_rax *>
    intermediate_to_asm64_ (Int64.succ non_let) y *>
    move_reg Reg_rax Reg_rbx *>
    pop Reg_rax *>
    sub Reg_rbx Reg_rax
  | IR2_Eq (x, y) ->
    intermediate_to_asm64_ non_let x *>
    push Reg_rax *>
    intermediate_to_asm64_ (Int64.succ non_let) y *>
    pop Reg_rbx *>
    tell (Op_cmp (Src_register Reg_rbx, Src_register Reg_rax))
  | IR2_And (x, y) ->
    intermediate_to_asm64_ non_let x *>
    push Reg_rax *>
    intermediate_to_asm64_ (Int64.succ non_let) y *>
    pop Reg_rbx *>
    and_ Reg_rbx Reg_rax

and intermediate_to_asm64_ (non_let: int64) : intermediate2 -> unit asm64_writer =
  function
  | IR2_PrimCall x ->
    primcall non_let x
  | IR2_Integer x ->
    integer x
  | IR2_Let (value, body) ->
    intermediate_to_asm64_ non_let value *>
    push Reg_rax *>
    intermediate_to_asm64_ non_let body *>
    drop 1L
  | IR2_Variable index ->
    move_from_offset ((non_let + index) * 8L) Reg_rsp Reg_rax
  | IR2_IfThenElse ite ->
    intermediate_to_asm64_ non_let ite.condition *>
    let else_label = unique_label () in
    let end_label = unique_label () in
    tell (Op_je else_label) *>
    intermediate_to_asm64_ non_let ite.branch2 *>
    tell (Op_jmp end_label) *>
    tell (Label else_label) *>
    intermediate_to_asm64_ non_let ite.branch1 *>
    tell (Label end_label)
  | IR2_Fail _ ->
    failwith "IR2 exceptions are not implemented (yet)"
  | IR2_MkArray args ->
    let size = Int64.of_int (List.length args) * 8L in (* each element is 8 bytes/64 bits *)
    allocate size *>
    store_into_array non_let args *>
    move_reg heap_pointer Reg_rax
  | IR2_ArrayIndex (index, array) ->
    intermediate_to_asm64_ non_let array *>
    move_from_offset (Int64.of_int index * 8L) Reg_rax Reg_rax

and store_into_array (non_let: int64) (args: intermediate2 list) : unit asm64_writer =
  List.foldi args ~init:writer_unit ~f:(fun i acc arg ->
    acc *>
    intermediate_to_asm64_ non_let arg *>
    move_to_offset Reg_rax (Int64.of_int i * 8L) heap_pointer
  )

let intermediate_to_asm64 (e: intermediate2) : unit asm64_writer =
  intermediate_to_asm64_ 0L e *>
  tell Op_ret
