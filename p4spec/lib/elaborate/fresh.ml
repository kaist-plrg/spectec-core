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
  let id = Il.Print.string_of_typ (exp.note $ exp.at) $ exp.at in
  let id = if wildcard then "_" ^ id.it $ id.at else id in
  fresh_id ids id

let fresh_from_plaintyp ?(wildcard = false) (ids : IdSet.t)
    (plaintyp : El.Ast.plaintyp) : Id.t =
  let id = El.Print.string_of_plaintyp plaintyp $ plaintyp.at in
  let id = if wildcard then "_" ^ id.it $ id.at else id in
  fresh_id ids id
