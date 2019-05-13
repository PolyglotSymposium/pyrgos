open Syntax

type data =
  { name : symbol
  ; isInf : bool (* whether the disjunction is infinite *)
  ; ctrs : symbol list
  }

let mkData (n, c) = { name = n ; isInf = false ; ctrs = c }

type gamma =
  { datas : data list
  ; exprs : (expr*texpr) list
  }

let isDataCtr (g : gamma) (v : symbol) : texpr option =
  let xs = List.find_all (fun x -> List.exists ((=) v) x.ctrs) g.datas
  in match xs with
     | [d] -> Some (TVar d.name)
     | _ -> None

exception DataCtrAlreadyDefined of symbol*texpr*texpr

let disallowShadowingCtrs (d : data) (g : gamma) : unit =
  (* None of these algorithms or data structures I'm building are going to scale
   * well... One thing I have not studied in compiler design is how to make them
   * efficient, lol. Modules could help here I suppose... Anyway, this is a
   * hack-it-up compiler for now, so have fun, lol! *)
  List.iter (fun ctr ->
    match isDataCtr g ctr with
    | None -> ()
    | Some existing ->
      let newT = TVar d.name in
      raise (DataCtrAlreadyDefined (ctr, newT, existing))
  ) d.ctrs

exception TypeAlreadyDefined of symbol

let disallowShadowingTypes (d : data) (g : gamma) : unit =
  let xs = List.find_all (fun x -> x.name = d.name) g.datas
  in match xs with
     | [] -> ()
     | _ -> raise (TypeAlreadyDefined d.name)

exception CtrsMustStartWithColon of symbol

let enforceCtrPrefix (d : data) : unit =
  List.iter (fun ctr ->
    match ctr.[0] with
    | ':' -> ()
    | _ -> raise (CtrsMustStartWithColon ctr)
  ) d.ctrs

let registerDataType (d : data) (g : gamma) : gamma =
  disallowShadowingCtrs d g;
  disallowShadowingTypes d g;
  enforceCtrPrefix d;
  (* TODO ensure uniqueness of constructor names within single type
   * definition *)
  { g with datas = d :: g.datas }

let mkGamma ds es =
  List.fold_right (mkData >> registerDataType) ds { datas = []; exprs = es }

let registerExprType (e : expr) (t : texpr) (g : gamma) : gamma =
  { g with exprs = (e, t) :: g.exprs }

let inGamma (g : gamma) (v : symbol) : texpr option =
  match List.find_all (fst >> (=) (Symbol v)) g.exprs with
  | [(_, t)] -> Some t
  | _ -> isDataCtr g v

