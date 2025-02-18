type register =
  | Reg_rax
  | Reg_rbx
  | Reg_rcx
  | Reg_rsp
  | Reg_al

let register_name : register -> string =
  function
  | Reg_rax -> "rax"
  | Reg_rbx -> "rbx"
  | Reg_rcx -> "rcx"
  | Reg_rsp -> "rsp"
  | Reg_al -> "al"

let format_register (reg : register) : string =
  Printf.sprintf "%%%s" (register_name reg)

(* TODO not convinced I have modeled this precisely *)
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

type label = Lbl of string

let format_label : label -> string =
  function
  | Lbl lbl -> lbl

(* TODO factor out by operation arity? *)
(* TODO have I modeled these at all decently? *)
type asm64 =
  | Label of label
  | Op_cmp of asm_value*asm_value
  | Op_test of asm_value*asm_value
  | Op_setz of register
  | Op_add of asm_value*register
  | Op_lea of asm_value*register
  | Op_pop of register
  | Op_je of label
  | Op_jmp of label
  | Op_push of asm_value
  | Op_sub of asm_value*register
  | Op_mov of asm_value*register
  | Op_ret

let emit_asm (out : out_channel): asm64 -> unit =
  function
  | Label lbl ->
    Printf.fprintf out "%s:\n" (format_label lbl)
  | Op_cmp (value1, value2) ->
    Printf.fprintf out "\tcmp %s, %s\n" (format_asm_value value1) (format_asm_value value2)
  | Op_test (value1, value2) ->
    Printf.fprintf out "\ttest %s, %s\n" (format_asm_value value1) (format_asm_value value2)
  | Op_add (value, reg) ->
    Printf.fprintf out "\tadd %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_lea (value, reg) ->
    Printf.fprintf out "\tlea %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_sub (value, reg) ->
    Printf.fprintf out "\tsub %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_mov (value, reg) ->
    Printf.fprintf out "\tmov %s, %s\n" (format_asm_value value) (format_register reg)
  | Op_push (value) ->
    Printf.fprintf out "\tpush %s\n" (format_asm_value value)
  | Op_jmp lbl ->
    Printf.fprintf out "\tjmp %s\n" (format_label lbl)
  | Op_je lbl ->
    Printf.fprintf out "\tje %s\n" (format_label lbl)
  | Op_setz reg ->
    Printf.fprintf out "\tsetz %s\n" (format_register reg)
  | Op_pop reg ->
    Printf.fprintf out "\tpop %s\n" (format_register reg)
  | Op_ret ->
    Printf.fprintf out "\tret\n"

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
