open Domain.Dom
open Util.Source

type t = IdSet.t

let fresh (ids : t) (id : Id.t) : Id.t =
  let ids =
    IdSet.filter
      (fun id_e ->
        let id = Xl.Var.strip_var_suffix id in
        let id_e = Xl.Var.strip_var_suffix id_e in
        id.it = id_e.it)
      ids
  in
  let rec fresh' (id : Id.t) : Id.t =
    if IdSet.mem id ids then fresh' (id.it ^ "'" $ id.at) else id
  in
  fresh' id
