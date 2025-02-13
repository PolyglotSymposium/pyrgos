type primcall =
  | Add of expr*expr
  | Sub of expr*expr

and expr =
  | PrimCall of primcall
  | Integer of int64
  | Let of string * expr * expr  (* Name, value, body *)
  | Variable of string
