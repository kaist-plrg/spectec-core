open Common.Source

module Ctx : sig
  type t
end

type error = region * string

val error_to_diagnostic : error -> Diag.t

val run :
  (module Target.S) ->
  Lang.Sl.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (Ctx.t * Lang.Il.Value.t list, error) result
