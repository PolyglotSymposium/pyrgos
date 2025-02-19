open Sexplib.Std
open Syntax

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
  [@@deriving sexp]

let match_not_exhaustive : intermediate =
  IR_Fail (Failure_builtin Match_not_exhaustive)

type environment = (string option) list

let rec expr_to_intermediate_ (expr : expr)
                              (env : environment) : intermediate =
  match expr with
  | Integer n -> IR_Integer n
  | Apply (Apply (InfixOp "+", e1), e2) ->
    let i1 = expr_to_intermediate_ e1 env
    and i2 = expr_to_intermediate_ e2 env
    in IR_PrimCall (IR_Add (i1, i2))
  | Apply (Apply (InfixOp "-", e1), e2) ->
    let i1 = expr_to_intermediate_ e1 env
    and i2 = expr_to_intermediate_ e2 env
    in IR_PrimCall (IR_Sub (i1, i2))
  | Apply (_, _) ->
    failwith "unimplemented case of apply"
  | InfixOp _ ->
    failwith "unexpected infix operator"
  | Let (Pattern_Wildcard, _value, body_expr) ->
    expr_to_intermediate_ body_expr env (* TODO execute effects in value? *)
  | Let (Pattern_Int _, _, _) ->
    fail_with_match_not_exhaustive ()
  | Let (Pattern_Var name, value_expr, body_expr) ->
    let value = expr_to_intermediate_ value_expr env
    and body = expr_to_intermediate_ body_expr (Some name :: env)
    in IR_Let (value, body)
  | Variable name ->
    (* TODO: handle shadowing *)
    begin
      match List.find_index (fun x -> x = Some name) env with
      | None -> failwith ("Unbound variable: " ^ name)
      | Some x -> IR_Variable (Int64.of_int x)
    end
  | Match m ->
    assert_exhaustive m;
    lower_match m env

and lower_match (m : match_with) (env : environment) : intermediate =
  let conditionals = List.fold_right (fun (p, body) acc ->
    match p with
    | Pattern_Int n ->
      let cond_ir = IR_PrimCall (IR_Eq (IR_Variable 0L, IR_Integer n)) in
      let body_ir = expr_to_intermediate_ body (None :: env) in
      IR_IfThenElse { condition = cond_ir; branch1 = body_ir; branch2 = acc }
    | Pattern_Wildcard ->
      expr_to_intermediate_ body env
    | Pattern_Var name ->
      (* the top-level let will effectively bind `name' *)
      expr_to_intermediate_ body (Some name :: env)
  ) m.cases match_not_exhaustive in
  let value_ir = expr_to_intermediate_ m.subject env in
  IR_Let (value_ir, conditionals)

let expr_to_intermediate (e : expr) : intermediate =
  expr_to_intermediate_ e []
