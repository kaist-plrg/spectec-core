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

let rec fresh_from_typ (at : region) (typ : Il.Ast.typ) :
    Id.t * Il.Ast.typ * Il.Ast.iter list =
  match typ.it with
  | IterT (typ, iter) ->
      let id, typ, iters = fresh_from_typ at typ in
      (id, typ, iters @ [ iter ])
  | _ ->
      let id = Il.Print.string_of_typ typ $ at in
      (id, typ, [])

let fresh_from_exp ?(wildcard = false) (ids : IdSet.t) (exp : Il.Ast.exp) :
    Id.t * Il.Ast.typ * Il.Ast.iter list =
  let id, typ, iters = fresh_from_typ exp.at (exp.note $ exp.at) in
  let id = if wildcard then "_" ^ id.it $ id.at else id in
  let id = fresh_id ids id in
  (id, typ, iters)

let fresh_from_plaintyp ?(wildcard = false) (ids : IdSet.t)
    (plaintyp : El.Ast.plaintyp) : Id.t =
  let id = El.Print.string_of_plaintyp plaintyp $ plaintyp.at in
  let id = if wildcard then "_" ^ id.it $ id.at else id in
  fresh_id ids id
