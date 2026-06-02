type error = Diag.t list
type 'a result = ('a, error) Stdlib.result

module Fresh : sig
  val fresh_id :
    Common.Domain.IdSet.t -> Common.Domain.Id.t -> Common.Domain.Id.t

  val fresh_var_from_exp :
    ?wildcard:bool -> Common.Domain.IdSet.t -> Lang.Il.exp -> Lang.Il.var

  val fresh_exp_from_typ :
    Common.Domain.IdSet.t -> Lang.Il.typ -> Lang.Il.exp * Common.Domain.IdSet.t
end

(** The elaborator's output: the language IL spec, plus per-backend IL slices.
*)
type il = { lang : Lang.Il.spec; qc : Qc_il.spec }

val elab_spec : Lang.El.spec -> il result
val error_to_diagnostics : error -> Diag.Bag.t
