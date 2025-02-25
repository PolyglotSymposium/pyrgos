open Sexplib.Std

type builtin_failure =
  | Match_not_exhaustive
  [@@deriving sexp]

type failure =
  | Failure_builtin of builtin_failure
  [@@deriving sexp]

(* TODO consider typed IR, like Haskell *)
type primcall =
  | IR2_Add of intermediate2 * intermediate2
  | IR2_Sub of intermediate2 * intermediate2
  | IR2_Eq of intermediate2 * intermediate2
  [@@deriving sexp]

and if_then_else = {
  condition : intermediate2;
  branch1 : intermediate2;
  branch2 : intermediate2;
} [@@deriving sexp]

and intermediate2 =
  | IR2_PrimCall of primcall
  | IR2_Integer of int64
  | IR2_Let of intermediate2 * intermediate2 (* Value, Body (no name) *)
  | IR2_Variable of int64 (* De Bruijn index *)
  | IR2_IfThenElse of if_then_else
  | IR2_Fail of failure
  | IR2_MkArray of intermediate2 list
  | IR2_ArrayIndex of int * intermediate2
  [@@deriving sexp]

let match_not_exhaustive : intermediate2 =
  IR2_Fail (Failure_builtin Match_not_exhaustive)
