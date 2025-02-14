type pattern =
  | Pattern_Int of int64
  | Pattern_Var of string
  | Pattern_Wildcard

(* TODO ultimately this should simply be subsumed by apply; this is too concrete
this far up the compiler *)
type apply_infix =
  | Add of expr*expr
  | Sub of expr*expr

and match_with = {
  subject : expr;
  cases : (pattern * expr) list
}

and expr =
  | ApplyInfix of apply_infix
  | Integer of int64
  | Let of string * expr * expr  (* Name, value, body *)
  | Variable of string
  | Match of match_with

let assert_exhaustive (m: match_with) : unit =
  (* No one wants to enumerate every case of int64 in a source file.\
     Treat it as not finitely enumerable. *)
  if List.exists (fun (p, _) ->
      match p with
      | Pattern_Wildcard -> true
      | _ -> false
  ) m.cases then ()
  else failwith "Pattern match not exhaustive"
