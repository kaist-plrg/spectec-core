module Builtins = Builtins
module Target = Target

type error =
  | EvalIlError of Eval_il.error
  | EvalSlError of Eval_sl.error
  | EvalPlError of Eval_pl.error

type ctx_il = Eval_il.Ctx.t
type ctx_sl = Eval_sl.Ctx.t
type ctx_pl = Eval_pl.Ctx.t

let error_to_diagnostic = function
  | EvalIlError e -> Eval_il.error_to_diagnostic e
  | EvalSlError e -> Eval_sl.error_to_diagnostic e
  | EvalPlError e -> Eval_pl.error_to_diagnostic e

let eval_il target spec rid args filename =
  Eval_il.run target spec rid args filename
  |> Result.map_error (fun e -> EvalIlError e)

let eval_sl target spec rid args filename =
  Eval_sl.run target spec rid args filename
  |> Result.map_error (fun e -> EvalSlError e)

let eval_pl target spec rid args filename =
  Eval_pl.run target spec rid args filename
  |> Result.map_error (fun e -> EvalPlError e)
