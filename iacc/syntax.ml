open Sexplib.Std

type pattern =
  | Pattern_Int of int64
  | Pattern_Var of string
  | Pattern_Wildcard
  [@@deriving sexp]

(* TODO ultimately this should simply be subsumed by apply; this is too concrete
this far up the compiler *)
type apply_infix =
  | Add of expr*expr
  | Sub of expr*expr
  [@@deriving sexp]

and match_with = {
  subject : expr;
  (* The list is in the order constructed naturally by parsing:
     that is, the farther down cases are closer to the head.  *)
  cases : (pattern * expr) list
} [@@deriving sexp]

and expr =
  | ApplyInfix of apply_infix
  | Integer of int64
  | Let of pattern * expr * expr  (* Name, value, body *)
  | Variable of string
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
