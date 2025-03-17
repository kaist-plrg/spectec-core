open Il.Ast

(* Entry point of dataflow analysis *)

(* Expression analysis *)

let analyze_exps_as_bind (ctx : Ctx.t) (exps : exp list) :
    Ctx.t * exp list * prem list =
  let dctx = Dctx.init ctx in
  let dctx, venv, exps, sideconditions =
    Binding.analyze_exps_as_bind dctx exps
  in
  let ctx = Dctx.promote ctx dctx venv in
  let exps = Dimension.analyze_exps ctx.venv exps in
  let sideconditions =
    Dimension.analyze_sideconditions ctx.venv sideconditions
  in
  (ctx, exps, sideconditions)

let analyze_exp_as_bound (ctx : Ctx.t) (exp : exp) : exp =
  let dctx = Dctx.init ctx in
  Binding.analyze_exp_as_bound dctx exp;
  Dimension.analyze_exp ctx.venv exp

let analyze_exps_as_bound (ctx : Ctx.t) (exps : exp list) : exp list =
  List.map (analyze_exp_as_bound ctx) exps

(* Argument analysis *)

let analyze_args_as_bind (ctx : Ctx.t) (args : arg list) :
    Ctx.t * arg list * prem list =
  let dctx = Dctx.init ctx in
  let dctx, venv, args, sideconditions =
    Binding.analyze_args_as_bind dctx args
  in
  let ctx = Dctx.promote ctx dctx venv in
  let args = Dimension.analyze_args ctx.venv args in
  let sideconditions =
    Dimension.analyze_sideconditions ctx.venv sideconditions
  in
  (ctx, args, sideconditions)

(* Premise analysis *)

let analyze_prem (ctx : Ctx.t) (prem : prem) : Ctx.t * prem * prem list =
  let dctx = Dctx.init ctx in
  let dctx, venv, prem, sideconditions = Binding.analyze_prem dctx prem in
  let ctx = Dctx.promote ctx dctx venv in
  let prem = Dimension.analyze_prem venv ctx.venv prem in
  let sideconditions =
    Dimension.analyze_sideconditions ctx.venv sideconditions
  in
  (ctx, prem, sideconditions)
