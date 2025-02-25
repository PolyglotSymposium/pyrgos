open Syntax
open Intermediate2

type environment = (string option) list

let rec expr_to_intermediate2_ (expr : expr)
                              (env : environment) : intermediate2 =
  match expr with
  | Integer n -> IR2_Integer n
  | Apply (Apply (InfixOp "+", e1), e2) ->
    let i1 = expr_to_intermediate2_ e1 env
    and i2 = expr_to_intermediate2_ e2 env
    in IR2_PrimCall (IR2_Add (i1, i2))
  | Apply (Apply (InfixOp "-", e1), e2) ->
    let i1 = expr_to_intermediate2_ e1 env
    and i2 = expr_to_intermediate2_ e2 env
    in IR2_PrimCall (IR2_Sub (i1, i2))
  | Apply (Apply (Constructor "Pair", e1), e2) ->
    let i1 = expr_to_intermediate2_ e1 env in
    let i2 = expr_to_intermediate2_ e2 env in
    IR2_MkArray [i1; i2]
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
    IR2-translation as a single-case match-with expression *)
    lower_match { subject = subject; cases = [(pattern, body)] } env
  | Variable name ->
    begin
      match List.find_index (fun x -> x = Some name) env with
      | None -> failwith ("Unbound variable: " ^ name)
      | Some x -> IR2_Variable (Int64.of_int x)
    end
  | Match m ->
    assert_exhaustive m;
    lower_match m env

and lower_match (m : match_with) (env : environment) : intermediate2 =
  let conditionals = List.fold_right (fun (p, body) acc ->
    match p with
    | Pattern_Int n ->
      let cond_ir = IR2_PrimCall (IR2_Eq (IR2_Variable 0L, IR2_Integer n)) in
      let body_ir = expr_to_intermediate2_ body (None :: env) in
      IR2_IfThenElse { condition = cond_ir; branch1 = body_ir; branch2 = acc }
    | Pattern_Wildcard ->
      expr_to_intermediate2_ body env
    | Pattern_Var name ->
      (* the top-level let will effectively bind `name' *)
      expr_to_intermediate2_ body (Some name :: env)
    | Pattern_Deconstruct ("Pair", [_pattern1; _pattern2]) ->
      (* Static typing should me we never get here unless we actually have a
        pair. Therefore, no dynamic type-checking is necessary here. *)
      failwith "TODO: implement Pair deconstruction"
    | Pattern_Deconstruct (_, _) ->
      failwith "Only 'Pair' deconstructor with two arguments is supported (for now)"
  ) m.cases match_not_exhaustive in
  let value_ir = expr_to_intermediate2_ m.subject env in
  IR2_Let (value_ir, conditionals)

let expr_to_intermediate2 (e : expr) : intermediate2 =
  expr_to_intermediate2_ e []
