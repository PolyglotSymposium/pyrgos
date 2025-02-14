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
  | Op_lea of asm_value*register
  | Op_pop of register
  | Op_push of asm_value
  | Op_sub of asm_value*register
  | Op_mov of asm_value*register
  | Op_ret

let emit_asm_ (out : out_channel): asm64 -> unit =
  function
  | Op_add (value, reg) ->
    Printf.fprintf out "add %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_lea (value, reg) ->
    Printf.fprintf out "lea %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_sub (value, reg) ->
    Printf.fprintf out "sub %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_mov (value, reg) ->
    Printf.fprintf out "mov %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_push (value) ->
    Printf.fprintf out "push %s\n" (format_asm_value value)
  | Op_pop (reg) ->
    Printf.fprintf out "pop %s\n" (format_register reg)
  | Op_ret ->
    Printf.fprintf out "ret\n"

let emit_asm (out : out_channel) (asm64 : asm64) : unit =
   Printf.fprintf out "\t";
   emit_asm_ out asm64

let emit_asm_header (out : out_channel) : unit =
  Printf.fprintf out "__entry__:\n"

let emit_asm_footer (out : out_channel) : unit =
  Printf.fprintf out ".LC0:\n";
  Printf.fprintf out "\t.string	\"%%d\\n\"\n";
  Printf.fprintf out "\t.globl	main\n";
  Printf.fprintf out "\t.type	main, @function\n";
  Printf.fprintf out "main:\n";
  Printf.fprintf out "\tcall __entry__\n";
  Printf.fprintf out "\tmovl	%%eax, %%esi\n";
  Printf.fprintf out "\tleaq	.LC0(%%rip), %%rdi\n";
  Printf.fprintf out "\txorl	%%eax, %%eax\n";
  Printf.fprintf out "\tcall	printf@PLT\n";
  Printf.fprintf out "\txorl	%%eax, %%eax\n";
  Printf.fprintf out "\tret\n"
