open Syntax

type primcall =
  | IR_Add of intermediate * intermediate
  | IR_Sub of intermediate * intermediate

and intermediate =
  | IR_PrimCall of primcall
  | IR_Integer of int64
  | IR_Let of intermediate * intermediate        (* Value, Body (no name) *)
  | IR_Variable of int64           (* De Bruijn index *)

type environment = string list

let rec expr_to_intermediate_ (expr : expr) (env : environment) : intermediate =
  match expr with
  | Integer n -> IR_Integer n
  | PrimCall (Add (e1, e2)) ->
    IR_PrimCall (IR_Add (expr_to_intermediate_ e1 env, expr_to_intermediate_ e2 env))
  | PrimCall (Sub (e1, e2)) ->
    IR_PrimCall (IR_Sub (expr_to_intermediate_ e1 env, expr_to_intermediate_ e2 env))
  | Let (name, value_expr, body_expr) ->
    IR_Let (expr_to_intermediate_ value_expr env, expr_to_intermediate_ body_expr (name :: env))
  | Variable name ->
    match List.find_index (fun x -> x = name) env with
    | None -> failwith ("Unbound variable: " ^ name)
    | Some x -> IR_Variable (Int64.of_int x)

let expr_to_intermediate (e : expr) : intermediate =
  expr_to_intermediate_ e []
