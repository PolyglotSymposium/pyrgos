type register =
  | Reg_eax
  | Reg_esp

let register_name : register -> string =
  function
  | Reg_eax -> "eax"
  | Reg_esp -> "esp"

let format_register (reg : register) : string =
  Printf.sprintf "%%%s" (register_name reg)

type asm_value =
  | Register of register
  | Offset of int*register
  | LitInt of int

let format_asm_value : asm_value -> string =
  function
  | Register reg ->
    format_register reg
  | Offset (offset, reg) ->
    Printf.sprintf "%i(%s)" offset (format_register reg)
  | LitInt int ->
    Printf.sprintf "$%i" int

(* TODO factor out by operation arity? *)
(* TODO why not use 64-bit instructions? *)
type asm64 =
  | Op_addl of asm_value*register
  | Op_subl of asm_value*register
  | Op_movl of asm_value*register
  | Op_ret

let emit_asm : asm64 -> unit =
  function
  | Op_addl (value, reg) ->
    Printf.printf "  addl %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_subl (value, reg) ->
    Printf.printf "  subl %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_movl (value, reg) ->
    Printf.printf "  movl %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_ret -> print_endline "  ret"
