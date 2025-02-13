type primcall =
  | Add of expr*expr
  | Sub of expr*expr

and expr =
  | PrimCall of primcall
  | Integer of int
