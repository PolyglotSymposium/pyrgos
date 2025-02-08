type primcall =
  | Add of int*int
  | Sub of int*int

type expr =
  | PrimCall of primcall
  | Integer of int
