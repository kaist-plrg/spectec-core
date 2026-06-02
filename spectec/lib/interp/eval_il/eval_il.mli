open Common.Source

module Ctx : sig
  type t
end

type error =
  | Plain of region * string
  | Backtrack of Common.Attempt.failtrace list

val error_to_diagnostic : error -> Diag.t

val run :
  (module Target.S) ->
  Lang.Il.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (Ctx.t * Lang.Il.Value.t list, error) result
