open Sexplib.Std

(* TODO file, line, column metadata annotations *)

type pattern =
  | Pattern_Int of int64
  | Pattern_Var of string
  | Pattern_Wildcard
  | Pattern_Deconstruct of string * pattern list
  [@@deriving sexp]

and match_with = {
  subject : expr;
  cases : (pattern * expr) list
} [@@deriving sexp]

and expr =
  | Apply of expr * expr
  | Integer of int64
  | Let of pattern * expr * expr  (* Name, value, body *)
  | Variable of string
  | InfixOp of string
  | Constructor of string
  | Match of match_with
  [@@deriving sexp]

let fail_with_match_not_exhaustive () =
  failwith "Pattern match not exhaustive"

let assert_exhaustive (m: match_with) : unit =
  (* No one wants to enumerate every case of int64 in a source file.\
     Treat it as not finitely enumerable. *)
  if List.exists (fun (p, _) ->
    match p with
    | Pattern_Wildcard -> true
    | _ -> false
  ) m.cases then ()
  else fail_with_match_not_exhaustive ()
