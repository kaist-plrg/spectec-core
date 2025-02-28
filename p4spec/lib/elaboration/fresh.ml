open Domain.Lib
open Util.Source

let fresh_id (ids : IdSet.t) (id : Id.t) : Id.t =
  let ids =
    IdSet.filter
      (fun id_e ->
        let id = Xl.Var.strip_var_suffix id in
        let id_e = Xl.Var.strip_var_suffix id_e in
        id.it = id_e.it)
      ids
  in
  let rec fresh_id' (id : Id.t) : Id.t =
    if IdSet.mem id ids then fresh_id' (id.it ^ "'" $ id.at) else id
  in
  fresh_id' id

let fresh_from_exp ?(wildcard = false) (ids : IdSet.t) (exp : Il.Ast.exp) : Id.t
    =
  let rec id_of_typ (typ : Il.Ast.typ) : Id.t =
    let at = typ.at in
    match typ.it with
    | BoolT -> "bool" $ at
    | NumT `NatT -> "nat" $ at
    | NumT `IntT -> "int" $ at
    | TextT -> "text" $ at
    | VarT (id, _) -> id
    | TupleT typs ->
        Format.asprintf "tuple(%s)"
          (String.concat ", "
             (typs |> List.map id_of_typ |> List.map Id.to_string))
        $ at
    | IterT (typ, iter) ->
        Format.asprintf "%s%s"
          (id_of_typ typ |> Id.to_string)
          (Il.Print.string_of_iter iter)
        $ at
  in
  let id = id_of_typ (exp.note $ exp.at) in
  let id = if wildcard then "_" ^ id.it $ id.at else id in
  fresh_id ids id

let fresh_from_plaintyp ?(wildcard = false) (ids : IdSet.t)
    (plaintyp : El.Ast.plaintyp) : Id.t =
  let rec id_of_plaintyp (plaintyp : El.Ast.plaintyp) : Id.t =
    let at = plaintyp.at in
    match plaintyp.it with
    | BoolT -> "bool" $ at
    | NumT `NatT -> "nat" $ at
    | NumT `IntT -> "int" $ at
    | TextT -> "text" $ at
    | VarT (id, _) -> id
    | ParenT plaintyp -> id_of_plaintyp plaintyp
    | TupleT plaintyps ->
        Format.asprintf "tuple(%s)"
          (String.concat ", "
             (plaintyps |> List.map id_of_plaintyp |> List.map Id.to_string))
        $ at
    | IterT (plaintyp, iter) ->
        Format.asprintf "%s%s"
          (id_of_plaintyp plaintyp |> Id.to_string)
          (El.Print.string_of_iter iter)
        $ at
  in
  let id = id_of_plaintyp plaintyp in
  let id = if wildcard then "_" ^ id.it $ id.at else id in
  fresh_id ids id
