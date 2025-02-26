open Intermediate1
open Intermediate2

type environment = (string option) list

let rec ir1_to_2_ (env : environment) : intermediate1 -> intermediate2 =
  function
  | IR1_Integer n -> IR2_Integer n
  | IR1_Apply (IR1_Apply (IR1_InfixOp "+", e1), e2) ->
    let i1 = ir1_to_2_ env e1
    and i2 = ir1_to_2_ env e2
    in IR2_PrimCall (IR2_Add (i1, i2))
  | IR1_Apply (IR1_Apply (IR1_InfixOp "-", e1), e2) ->
    let i1 = ir1_to_2_ env e1
    and i2 = ir1_to_2_ env e2
    in IR2_PrimCall (IR2_Sub (i1, i2))
  | IR1_Apply (IR1_Apply (IR1_Constructor "Pair", e1), e2) ->
    let i1 = ir1_to_2_ env e1
    and i2 = ir1_to_2_ env e2
    in IR2_MkArray [i1; i2]
  | IR1_Apply (_, _) ->
    failwith "unimplemented case of apply"
  | IR1_InfixOp _ ->
    failwith "unexpected infix operator"
  | IR1_Constructor _ ->
    failwith "unexpected constructor"
  | IR1_Variable name ->
    begin
      match List.find_index (fun x -> x = Some name) env with
      | None -> failwith ("Unbound variable: " ^ name)
      | Some x -> IR2_Variable (Int64.of_int x)
    end
  | IR1_Match m ->
    assert_exhaustive m;
    lower_match m env

and lower_match (m : flat_match) (env : environment) : intermediate2 =
  let conditionals = List.fold_right (fun (p, body) acc ->
    match p with
    | IR1_Pat_Terminal (TPat_Int n) ->
      let cond_ir = IR2_PrimCall (IR2_Eq (IR2_Variable 0L, IR2_Integer n)) in
      let body_ir = ir1_to_2_ (None :: env) body in
      IR2_IfThenElse { condition = cond_ir; branch1 = body_ir; branch2 = acc }
    | IR1_Pat_Terminal TPat_Wildcard ->
      ir1_to_2_ env body
    | IR1_Pat_Terminal (TPat_Var name) ->
      (* the top-level let will effectively bind `name' *)
      ir1_to_2_ (Some name :: env) body
    | IR1_Pat_Deconstruct { head_pattern = { deconstructor = "Pair"; args = [_pattern1; _pattern2]}; with_patterns = _ } ->
      (* Static typing should me we never get here unless we actually have a
        pair. Therefore, no dynamic type-checking is necessary here. *)
      failwith "TODO: implement Pair deconstruction"
    | IR1_Pat_Deconstruct _ ->
      failwith "Only 'Pair' deconstructor with two arguments is supported (for now)"
  ) m.cases match_not_exhaustive in
  let value_ir = ir1_to_2_ env m.subject in
  IR2_Let (value_ir, conditionals)

let ir1_to_2 (e : intermediate1) : intermediate2 =
  ir1_to_2_ [] e
