open Syntax
open Intermediate

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
  | Apply (Apply (Constructor "Pair", e1), e2) ->
    let i1 = expr_to_intermediate_ e1 env in
    let i2 = expr_to_intermediate_ e2 env in
    IR_MkArray [i1; i2]
  | Apply (_, _) ->
    failwith "unimplemented case of apply"
  | InfixOp _ ->
    failwith "unexpected infix operator"
  | Constructor _ ->
    failwith "unexpected constructor"
  | Let (Pattern_Int _, _, _) ->
    fail_with_match_not_exhaustive ()
  | Let (pattern, subject, body) ->
    (* A let expression with an exhaustive pattern match has the same
    IR-translation as a single-case match-with expression *)
    lower_match { subject = subject; cases = [(pattern, body)] } env
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
    | Pattern_Deconstruct ("Pair", [_pattern1; _pattern2]) ->
      (* Static typing should me we never get here unless we actually have a
        pair. Therefore, no dynamic type-checking is necessary here. *)
      failwith "TODO: implement Pair deconstruction"
    | Pattern_Deconstruct (_, _) ->
      failwith "Only 'Pair' deconstructor with two arguments is supported (for now)"
  ) m.cases match_not_exhaustive in
  let value_ir = expr_to_intermediate_ m.subject env in
  IR_Let (value_ir, conditionals)

let expr_to_intermediate (e : expr) : intermediate =
  expr_to_intermediate_ e []
