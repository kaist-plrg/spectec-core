open Common.Domain
open Common.Source
open Types

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

let rec fresh_var_from_typ (at : region) (typ : typ) : var =
  match typ.it with
  | IterT { typ; iter } ->
      let var = fresh_var_from_typ at typ in
      { var with iters = var.iters @ [ iter ] }
  | _ ->
      let varid = Print.string_of_typ typ $ at in
      { varid; typ; iters = [] }

let fresh_var_from_exp ?(wildcard = false) (ids : IdSet.t) (exp : exp) : var =
  let var = fresh_var_from_typ exp.at (exp.note $ exp.at) in
  let varid =
    if wildcard then "_" ^ var.varid.it $ var.varid.at else var.varid
  in
  let varid = fresh_id ids varid in
  { var with varid }

let fresh_exp_from_typ (ids : IdSet.t) (typ : typ) : exp * IdSet.t =
  let { varid = id_base; typ = typ_base; iters } =
    fresh_var_from_typ typ.at typ
  in
  let id_base = fresh_id ids id_base in
  let ids = IdSet.add id_base ids in
  let exp_base = VarE id_base $$ (typ_base.at, typ_base.it) in
  let exp_match, _ =
    List.fold_left
      (fun (exp_match, iters) iter ->
        let typ = IterT { typ = exp_match.note $ exp_match.at; iter } in
        let var = { varid = id_base; typ = typ_base; iters } in
        let iterexp = (iter, [ var ]) in
        let exp_match = IterE (exp_match, iterexp) $$ (exp_match.at, typ) in
        (exp_match, iters @ [ iter ]))
      (exp_base, []) iters
  in
  (exp_match, ids)
