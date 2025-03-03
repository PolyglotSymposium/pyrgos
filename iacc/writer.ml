open Assembly

type 'a asm64_writer =
  | Asm64_writer of asm64 list * 'a

let writer_unit  : unit asm64_writer =
  Asm64_writer ([], ())

let tell (x : asm64) : unit asm64_writer =
  Asm64_writer ([x], ())

let ( *> ) fa fb =
  let Asm64_writer (log1, _) = fa
  and Asm64_writer (log2, x) = fb
  in Asm64_writer (List.append log1 log2, x)

let emit_all_asm (out : out_channel) (fa : unit asm64_writer) : unit =
  emit_asm_header out;
  let (Asm64_writer (lines, ())) = fa
  in List.iter (emit_asm out) lines;
  emit_asm_footer out
