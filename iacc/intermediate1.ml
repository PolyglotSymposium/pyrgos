open Sexplib.Std

(* TODO file, line, column metadata annotations *)

type deconstruct_pattern = {
  deconstructor: string;
  args: Syntax.terminal_pattern list;
} [@@deriving sexp]

type flat_pattern =
  | Flat_Pat_Terminal of Syntax.terminal_pattern
  | Flat_Pat_Deconstruct of deconstruct_pattern
  [@@deriving sexp]

(* The idea here is transform something like

     | Pair (Pair x _) (Pair y 5) -> ...

  into something like (contrived syntax)

     | Pair a b with a matches Pair x _ with b matches Pair y 5 -> ...
*)
type flat_patterns = {
  head_pattern: deconstruct_pattern;
  (* `with` patterns have the structure: variable * pattern
     with patterns are never terminal, because if they were terminal, we would
     not bother to extract them *)
  with_patterns: (string * deconstruct_pattern) list;
} [@@deriving sexp]

type ir1_pattern =
  | IR1_Pat_Terminal of Syntax.terminal_pattern
  | IR1_Pat_Deconstruct of flat_patterns
  [@@deriving sexp]

type flat_match = {
  subject : intermediate1;
  cases : (ir1_pattern * intermediate1) list
} [@@deriving sexp]

and intermediate1 =
  | IR1_Apply of intermediate1 * intermediate1
  | IR1_Integer of int64
  | IR1_Variable of string
  | IR1_InfixOp of string
  | IR1_Constructor of string
  (* No `let` expression because a let expression is equivalent (by operational
    semantics) to a single-case match-with expression *)
  | IR1_Match of flat_match
  [@@deriving sexp]

let fail_with_match_not_exhaustive () =
  failwith "Pattern match not exhaustive"

let assert_exhaustive (_m: flat_match) : unit =
  (* No one wants to enumerate every case of int64 in a source file.
     Treat it as not finitely enumerable. *)
  () (* TODO support pairs in exhaustivity check; but at this level, or in syntax? *)
(*
  if List.exists (fun (p, _) ->
    match p with
    | IR1_Pat_Terminal tpat ->
      Syntax.is_catchall tpat
    | IR1_Pat_Deconstruct { deconstructor = _; args} ->
      List.for_all Syntax.is_catchall args
  ) m.cases then ()
  else fail_with_match_not_exhaustive ()
*)
