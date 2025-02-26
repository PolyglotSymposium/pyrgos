open Base
open Base.Int64
open Syntax
open Intermediate1

let stage1_var_counter = ref 0L

let unique_stage1_var () : string =
  let x = !stage1_var_counter in
  stage1_var_counter := x + 1L;
  (* The surface syntax does not allow variables to begin with $ *)
  Printf.sprintf "$stage1_%Ld" x

let flatten_arg (arg: pattern) : terminal_pattern * (string * (string * pattern list)) option =
  match arg with
  | Pattern_Terminal tpat -> (tpat, None)
  | Pattern_Deconstruct (dctor, args) ->
    let fresh_var = unique_stage1_var ()
    in (TPat_Var fresh_var, Some (fresh_var, (dctor, args)))

let rec flatten_args (args: pattern list) : terminal_pattern list * (string * deconstruct_pattern) list =
  let (tpats, options) = List.unzip (List.map ~f:flatten_arg args) in
  let with_patterns =
    List.filter_map ~f:(fun x -> x) options
    |> List.bind ~f:(fun (var, (dctor, args_)) ->
       let (flat_args, more_with_patterns) = flatten_args args_
       in (var, { deconstructor = dctor; args = flat_args })
          :: more_with_patterns
    )
  in (tpats, with_patterns)

let flatten_deconstruct (dctor: string) (args: pattern list) : flat_patterns =
  let (flat_args, with_patterns) = flatten_args args
  in {
    head_pattern = {
      deconstructor = dctor;
      args = flat_args;
    };
    with_patterns = with_patterns;
  }

let flatten_pattern (pat : pattern) : ir1_pattern =
  match pat with
  | Pattern_Terminal tpat -> IR1_Pat_Terminal tpat
  | Pattern_Deconstruct (dctor, args) ->
    IR1_Pat_Deconstruct (flatten_deconstruct dctor args)

let rec flatten_case ((pat, body) : pattern * expr) : ir1_pattern * intermediate1 =
  let ir1_body = expr_to_ir1 body
  and ir1_pattern = flatten_pattern pat
  in (ir1_pattern, ir1_body)

and flatten_match (m : match_with) : flat_match =
  {
    subject = expr_to_ir1 m.subject;
    cases = List.map ~f:flatten_case m.cases;
  }

and expr_to_ir1 (e : expr) : intermediate1 =
  match e with
  | Integer n -> IR1_Integer n
  | Variable name -> IR1_Variable name
  | InfixOp op -> IR1_InfixOp op
  | Constructor c -> IR1_Constructor c
  | Apply (e1, e2) -> IR1_Apply (expr_to_ir1 e1, expr_to_ir1 e2)
  | Let (pattern, value, body) ->
    IR1_Match {
      subject = expr_to_ir1 value;
      cases = [flatten_case (pattern, body)]
    }
  | Match m ->
    IR1_Match (flatten_match m)
