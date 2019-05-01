open Syntax

type data =
  { name : symbol
  ; ctrs : symbol list
  }

let mkData n c = { name = n ; ctrs = c }

type datas = data list

let mkDatas = List.map (fun (n, c) -> mkData n c)

type gamma =
  { datas : data list
  ; exprs : (expr*texpr) list
  }

let mkGamma ds es =
  { datas = mkDatas ds; exprs = es }

let registerDataType (d : data) (g : gamma) : gamma =
  (* TODO what safety checks are needed here? *)
  { g with datas = d :: g.datas }

let registerExprType (e : expr) (t : texpr) (g : gamma) : gamma =
  { g with exprs = (e, t) :: g.exprs }

let isDataCtr (g : gamma) (v : symbol) : texpr option =
  let xs = List.find_all (fun x -> List.exists ((=) v) x.ctrs) g.datas
  in match xs with
     | [d] -> Some (TVar d.name)
     | _ -> None

let inGamma (g : gamma) (v : symbol) : texpr option =
  match List.find_all (fst >> (=) (Symbol v)) g.exprs with
  | [(_, t)] -> Some t
  | _ -> isDataCtr g v

