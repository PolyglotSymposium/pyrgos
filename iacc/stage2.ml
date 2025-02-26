open Base
open Base.String
open Intermediate1
open Intermediate2

type environment = (string option) list

type array_elem_eq_cond = {
  index: int;
  value: intermediate2;
}

let compile_array_elem_eq_cond { index; value } =
  IR2_PrimCall (IR2_Eq (IR2_ArrayIndex (index, IR2_Variable 0L), value))

type array_elem_binding = {
  index: int;
  name: string;
}

let compile_array_elem_binding depth { index; name } =
  (name, IR2_ArrayIndex (index, IR2_Variable (Int64.of_int depth)))

type dctor_arg_match = {
  conditions: array_elem_eq_cond list;
  bindings: array_elem_binding list;
}

let empty_dctor_arg_match = { conditions = []; bindings = [] }

let combine_dctor_arg_matches
    (m1: dctor_arg_match) (m2: dctor_arg_match): dctor_arg_match =
  {
    conditions = m1.conditions @ m2.conditions;
    bindings = m1.bindings @ m2.bindings
  }

let arg_to_dctor_arg_match (i: int) (tpat: Syntax.terminal_pattern): dctor_arg_match =
  match tpat with
  | TPat_Int n ->
    { empty_dctor_arg_match with conditions = [{ index = i; value = IR2_Integer n }] }
  | TPat_Wildcard ->
    empty_dctor_arg_match
  | TPat_Var name ->
    { empty_dctor_arg_match with bindings = [{ index = i; name }] }

let add_conditional_dctor_arg_match
    (m: dctor_arg_match) (body: intermediate2)
    (acc: intermediate2): intermediate2 =
  let cond_ir =
    List.map ~f:compile_array_elem_eq_cond m.conditions
    |> List.reduce ~f:(fun x y -> IR2_PrimCall (IR2_And (x, y)))
  in match cond_ir with
  | Some cond_ir ->
    IR2_IfThenElse { condition = cond_ir; branch1 = body; branch2 = acc }
  | None -> body

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
      match List.findi env ~f:(fun _ (x: string option) -> Option.exists x ~f:(equal name)) with
      | None -> failwith ("Unbound variable: " ^ name)
      | Some (x, _) -> IR2_Variable (Int64.of_int x)
    end
  | IR1_Match m ->
    assert_exhaustive m;
    lower_match m env

and lower_match (m : flat_match) (env : environment) : intermediate2 =
  let conditionals = List.fold_right ~f:(fun (p, body) acc ->
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
    | IR1_Pat_Deconstruct _fpats ->
       (* { head_pattern = { deconstructor = "Pair"; args = [_pattern1; _pattern2]}; with_patterns = _ } *)
      (* Static typing should me we never get here unless we actually have a
        pair. Therefore, no dynamic type-checking is necessary here. *)
      failwith "TODO: implement Pair deconstruction"
  ) m.cases ~init:match_not_exhaustive in
  let value_ir = ir1_to_2_ env m.subject in
  IR2_Let (value_ir, conditionals)

(* Examples:
   `Pair 5 6` ~> `if (5 = fst $0) and (6 = snd $1) then body else acc`
   `Pair x y` ~> `let x = fst $0; let y = snd $1; body`
   `Pair _ y` ~> `let y = snd $0; body`
   `Pair x _` ~> `let x = fst $0; body`
   `Pair 5 _` ~> `if (5 = fst $0) then body else acc`
   `Pair x 6` ~> `if (6 = snd $0) then let x = fst $0; body else acc`
   `Pair 5 y` ~> `if (5 = fst $0) then let y = snd $0; body else acc`

   If there is a conditional, it is always on the outside, and there is no need
   to nested it; the conditions may be `and`-ed together.

   If there is a let expression, it comes inside the conditional (if any), and
   it may be nested.
*)
and lower_deconstructor_match (dpat: deconstruct_pattern) (_env: environment) : intermediate2 =
  match dpat.deconstructor with
  | "Pair" ->
    begin
      match dpat.args with
      | [_p1; _p2] ->
        failwith "TODO"
      | _ -> failwith "Pair deconstructor must have exactly two arguments"
    end
  | _ -> failwith "unimplemented deconstructor"

and compile_bindings_dctor_arg_match
    (env: environment) (m: dctor_arg_match)
    (body: intermediate1): intermediate2 =
  let (names, values) =
    List.mapi ~f:compile_array_elem_binding m.bindings
    |> List.unzip
  in let new_env = (List.map ~f:(fun x -> Some x) names) @ env
  in let body_ir2 = ir1_to_2_ new_env body
  in List.fold_right ~f:(fun value acc -> IR2_Let (value, acc)) ~init:body_ir2 values

and compile_dctor_arg_match
    (env: environment) (m: dctor_arg_match) (body: intermediate1)
    (acc: intermediate2): intermediate2 =
  let body_ir2 = compile_bindings_dctor_arg_match env m body
  in add_conditional_dctor_arg_match m body_ir2 acc

let ir1_to_2 (e : intermediate1) : intermediate2 =
  ir1_to_2_ [] e
