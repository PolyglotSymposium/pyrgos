open Sexplib.Std

type builtin_failure =
  | Match_not_exhaustive
  [@@deriving sexp]

type failure =
  | Failure_builtin of builtin_failure
  [@@deriving sexp]

(* TODO consider typed IR, like Haskell *)
type primcall =
  | IR_Add of intermediate * intermediate
  | IR_Sub of intermediate * intermediate
  | IR_Eq of intermediate * intermediate
  [@@deriving sexp]

and if_then_else = {
  condition : intermediate;
  branch1 : intermediate;
  branch2 : intermediate;
} [@@deriving sexp]

and intermediate =
  | IR_PrimCall of primcall
  | IR_Integer of int64
  | IR_Let of intermediate * intermediate (* Value, Body (no name) *)
  | IR_Variable of int64 (* De Bruijn index *)
  | IR_IfThenElse of if_then_else
  | IR_Fail of failure
  | IR_MkArray of intermediate list
  | IR_ArrayIndex of int * intermediate
  [@@deriving sexp]

let match_not_exhaustive : intermediate =
  IR_Fail (Failure_builtin Match_not_exhaustive)
