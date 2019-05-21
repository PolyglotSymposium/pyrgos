open Util
open Syntax
open Types

let rec removeFromList (elem : 'a) : 'a list -> 'a list option =
  function
  | [] -> None
  | x :: xs ->
    if x = elem
    then Some xs
    else match removeFromList elem xs with
         | None -> None
         | Some xs' -> Some (x :: xs')

let match1Ctr (e : expr) : expr list option -> bool*(expr list option) =
  function
  | None -> true, None
  | Some ctrs ->
    match removeFromList e ctrs with
    | None -> false, Some ctrs
    | Some ctrs' -> true, Some ctrs'

let dataCtrs (g : gamma) : expr -> expr list option =
  function
  | DataCtr dc ->
    typeOfDataCtr g dc
    |> map_opt (fun d ->
      List.map (fun x -> DataCtr x) (
        match removeFromList dc d.ctrs with
        | None -> d.ctrs
        | Some ctrs -> ctrs
      )
    )
  | _ -> None

let rec check (g : gamma) (e : expr) (t : texpr) : bool =
  match e with
  | Lambda [Symbol arg, body] ->
    (match t with
    | Func (t1, t2) ->
      let g' = registerExprType (Symbol arg) t1 g
      in check g' body t2
    | _ -> false (* We have no type aliasing mechanism *))
  | _ -> synthesize g e = Some t

and synthesize (g : gamma) : expr -> texpr option =
  function
  | Appl (f, x) -> synthAppl g f x
  | Atom _ -> Some Prelude.tAtom (* The Atom type is an infinite disjunction. *)
  | DataCtr x -> isDataCtr g x
  | Lambda ((Symbol _, _) :: _) -> None
  | Lambda ((a, b) :: pieces) ->
    let aType = synthesize g a in
    let bType = synthesize g b in
    (match (aType, bType) with
    | (Some at, Some bt) ->
      let ctrs = dataCtrs g a in
      if checkPieces g at bt ctrs pieces
      then Some (Func (at, bt))
      else None
    | _ -> None)
  | Lambda [] -> None
  | List _ -> Some Prelude.tList
  | Quote _ -> Some Prelude.tExpr
  | Symbol v -> inGamma g v
  | TExpr _ -> Some Prelude.tTypeExpr

and synthAppl (g : gamma) (f : expr) (x : expr) : texpr option =
  match synthesize g f with
  | Some (Func (t1, t2)) -> if check g x t1 then Some t2 else None
  | _ -> None

and checkPieces
    (g : gamma) (argT : texpr) (bodyT : texpr)
    (ctrs : expr list option) (pieces : (expr*expr) list)
    : bool =
  match pieces with
  | [] -> ctrs = Some []
  | [Symbol a, b] ->
    let g' = registerExprType (Symbol a) argT g
    in check g' b bodyT
  | (a, b) :: pieces' ->
    let (matched,ctrs') = match1Ctr a ctrs in
    matched
    && check g a argT
    && check g b bodyT
    && checkPieces g argT bodyT ctrs' pieces'
