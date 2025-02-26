open Sexplib.Std

(* TODO file, line, column metadata annotations *)

type terminal_pattern =
  | TPat_Int of int64
  | TPat_Var of string
  | TPat_Wildcard
  [@@deriving sexp]

let is_catchall : terminal_pattern -> bool =
  function
  | TPat_Wildcard -> true
  | TPat_Var _ -> true
  | _ -> false

type pattern =
  | Pattern_Terminal of terminal_pattern
  | Pattern_Deconstruct of string * pattern list
  [@@deriving sexp]

type match_with = {
  subject : expr;
  cases : (pattern * expr) list
} [@@deriving sexp]

and expr =
  | Apply of expr * expr
  | Integer of int64
  | Let of pattern * expr * expr  (* pattern, value, body *)
  | Variable of string
  | InfixOp of string
  | Constructor of string
  | Match of match_with
  [@@deriving sexp]
