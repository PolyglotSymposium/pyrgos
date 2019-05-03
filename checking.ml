open Syntax
open Types

let rec check (g : gamma) (e : expr) (t : texpr) : bool =
  match e with
  | Lambda [arg, body] ->
    (match t with
    | Func (t1, t2) ->
      (* TODO actually implement pattern matching *)
      let g' = if arg = "_" || arg.[0] = ':'
               then g (* don't bind constructors and _ *)
               else registerExprType (Symbol arg) t1 g
      in check g' body t2
    | _ -> false (* We have no type aliasiang mechanism *))
  | Lambda _ -> failwith "NOT IMPLEMENT YET"
  | _ -> synthesize g e = Some t

and synthesize (g : gamma) : expr -> texpr option =
  function
  | Appl (f, x) -> synthAppl g f x
  | Atom _ -> Some Prelude.tAtom (* The Atom type is an infinite disjunction. *)
  | Lambda _ -> None (* There is no synthesis rule for lambdas *)
  | List _ -> Some Prelude.tList
  | Quote _ -> Some Prelude.tExpr
  | Symbol v -> inGamma g v
  | TExpr _ -> Some Prelude.tTypeExpr

and synthAppl (g : gamma) (f : expr) (x : expr) : texpr option =
  match synthesize g f with
  | Some (Func (t1, t2)) -> if check g x t1 then Some t2 else None
  | _ -> None
