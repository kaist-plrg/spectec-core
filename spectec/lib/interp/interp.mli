module Builtins = Builtins
module Target = Target

type error
type ctx_il
type ctx_sl
type ctx_pl

val error_to_diagnostic : error -> Diag.t

val eval_il :
  (module Target.S) ->
  Lang.Il.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (ctx_il * Lang.Il.Value.t list, error) result

val eval_sl :
  (module Target.S) ->
  Lang.Sl.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (ctx_sl * Lang.Il.Value.t list, error) result

val eval_pl :
  (module Target.S) ->
  Lang.Pl.spec ->
  string ->
  Lang.Il.Value.t list ->
  string ->
  (ctx_pl * Lang.Il.Value.t list, error) result
