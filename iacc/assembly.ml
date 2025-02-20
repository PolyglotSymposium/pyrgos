type register =
  | Reg_rax
  | Reg_rbx
  | Reg_rcx
  | Reg_rsp
  | Reg_esi
  | Reg_al

let register_name : register -> string =
  function
  | Reg_rax -> "rax"
  | Reg_rbx -> "rbx"
  | Reg_rcx -> "rcx"
  | Reg_rsp -> "rsp"
  | Reg_esi -> "esi"
  | Reg_al -> "al"

let format_register (reg : register) : string =
  Printf.sprintf "%%%s" (register_name reg)

(* TODO not convinced I have modeled this precisely *)
type asm_src =
  | Src_register of register
  | Src_offset of int64*register
  | LitInt64 of int64

let format_asm_src : asm_src -> string =
  function
  | Src_register reg ->
    format_register reg
  | Src_offset (offset, reg) ->
    Printf.sprintf "%Ld(%s)" offset (format_register reg)
  | LitInt64 x ->
    Printf.sprintf "$%Ld" x

(* TODO not convinced I have modeled this precisely *)
type asm_dst =
  | Dst_register of register
  | Dst_offset of int64*register

let format_asm_dst : asm_dst -> string =
  function
  | Dst_register reg ->
    format_register reg
  | Dst_offset (offset, reg) ->
    Printf.sprintf "%Ld(%s)" offset (format_register reg)

type label = Lbl of string

let format_label : label -> string =
  function
  | Lbl lbl -> lbl

(* TODO factor out by operation arity? *)
(* TODO have I modeled these at all decently? *)
type asm64 =
  | Label of label
  | Op_cmp of asm_src*asm_src
  | Op_add of asm_src*asm_dst
  | Op_lea of asm_src*register
  | Op_pop of register
  | Op_je of label
  | Op_jmp of label
  | Op_push of asm_src
  | Op_sub of asm_src*asm_dst
  | Op_mov of asm_src*asm_dst
  | Op_ret

let emit_asm (out : out_channel): asm64 -> unit =
  function
  | Label lbl ->
    Printf.fprintf out "%s:\n" (format_label lbl)
  | Op_cmp (value1, value2) ->
    Printf.fprintf out "\tcmp %s, %s\n" (format_asm_src value1) (format_asm_src value2)
  | Op_add (src, dst) ->
    Printf.fprintf out "\tadd %s, %s\n" (format_asm_src src) (format_asm_dst dst)
  | Op_lea (value, reg) ->
    Printf.fprintf out "\tlea %s, %s\n" (format_asm_src value) (format_register reg)
  | Op_sub (src, dst) ->
    Printf.fprintf out "\tsub %s, %s\n" (format_asm_src src) (format_asm_dst dst)
  | Op_mov (src, dst) ->
    Printf.fprintf out "\tmov %s, %s\n" (format_asm_src src) (format_asm_dst dst)
  | Op_push (value) ->
    Printf.fprintf out "\tpush %s\n" (format_asm_src value)
  | Op_jmp lbl ->
    Printf.fprintf out "\tjmp %s\n" (format_label lbl)
  | Op_je lbl ->
    Printf.fprintf out "\tje %s\n" (format_label lbl)
  | Op_pop reg ->
    Printf.fprintf out "\tpop %s\n" (format_register reg)
  | Op_ret ->
    Printf.fprintf out "\tret\n"

let emit_asm_header (out : out_channel) : unit =
  Printf.fprintf out "__entry__:\n";
  Printf.fprintf out "\tmov $10485760, %%rdi\n"; (* 10MB *)
  Printf.fprintf out "\tmov $12, %%rax\n"; (* sbrk syscall number (Linux) *)
  Printf.fprintf out "\tsyscall\n";
  Printf.fprintf out "\tcmp $-1, %%rax\n"; (* Check for sbrk error (-1 return means error) *)
  Printf.fprintf out "\tje heap_allocation_error\n"; (* Jump to error handler if sbrk fails*)
  Printf.fprintf out "\tmov %%rax, %%esi\n" (*; Store the start of the 10MB heap in %esi *)

let emit_asm_footer (out : out_channel) : unit =
  Printf.fprintf out "heap_allocation_error:\n";
  Printf.fprintf out "\tmov $1, %%rdi\n"; (* Error code *)
  Printf.fprintf out "\tmov $60, %%rax\n"; (* exit syscall number (Linux) *)
  Printf.fprintf out "\tsyscall\n";
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
