open Domain.Lib
open Il.Ast
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

let rec id_of_typ (typ : typ) : Id.t =
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

let fresh_exp (ids : IdSet.t) (exp : exp) : Id.t =
  let id = id_of_typ (exp.note $ exp.at) in
  fresh_id ids id
