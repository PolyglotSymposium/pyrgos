type register =
  | Reg_rax
  | Reg_rbx
  | Reg_rcx
  | Reg_rsp

let register_name : register -> string =
  function
  | Reg_rax -> "rax"
  | Reg_rbx -> "rbx"
  | Reg_rcx -> "rcx"
  | Reg_rsp -> "rsp"

let format_register (reg : register) : string =
  Printf.sprintf "%%%s" (register_name reg)

type asm_value =
  | Register of register
  | Offset of int64*register
  | LitInt64 of int64

let format_asm_value : asm_value -> string =
  function
  | Register reg ->
    format_register reg
  | Offset (offset, reg) ->
    Printf.sprintf "%Ld(%s)" offset (format_register reg)
  | LitInt64 x ->
    Printf.sprintf "$%Ld" x

(* TODO factor out by operation arity? *)
(* TODO why not use 64-bit instructions? *)
type asm64 =
  | Op_add of asm_value*register
  | Op_pop of register
  | Op_push of asm_value
  | Op_sub of asm_value*register
  | Op_mov of asm_value*register
  | Op_ret

let emit_asm : asm64 -> unit =
  function
  | Op_add (value, reg) ->
    Printf.printf "  add %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_sub (value, reg) ->
    Printf.printf "  sub %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_mov (value, reg) ->
    Printf.printf "  mov %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_push (value) ->
    Printf.printf "  push %s\n" (format_asm_value value)
  | Op_pop (reg) ->
    Printf.printf "  pop %s\n" (format_register reg)
  | Op_ret -> print_endline "  ret"
