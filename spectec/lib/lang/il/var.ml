open Common.Source
open Types

(** [as_exp var] rebuilds the expression form of [var], wrapping one [IterE] per
    iteration dimension. *)
let as_exp ({ varid; typ; iters } : var) : exp =
  let exp_base = VarE varid $$ (varid.at, typ.it) in
  let exp, _ =
    List.fold_left
      (fun (exp, iters_below) iter ->
        let typ' = IterT { typ = exp.note $ exp.at; iter } in
        let iterexp = (iter, [ { varid; typ; iters = iters_below } ]) in
        let exp = IterE (exp, iterexp) $$ (exp.at, typ') in
        (exp, iters_below @ [ iter ]))
      (exp_base, []) iters
  in
  exp
