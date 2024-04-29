module TDEnv = Ds.Env (Type)

module Env = Ds.AEnv

module Sto = struct
  module TSto = Ds.Heap (Type)
  module VSto = Ds.Heap (Value)
  type t = TSto.t * VSto.t

  let empty = (TSto.empty, VSto.empty)

  let find addr (tsto, vsto) =
    let typ = TSto.find addr tsto in
    let value = VSto.find addr vsto in
    (typ, value)

  let add addr typ value (tsto, vsto) =
    let tsto = TSto.add addr typ tsto in
    match value with
    | None -> (tsto, vsto)
    | Some value ->
      let vsto = VSto.add addr value vsto in
      (tsto, vsto)

  let update addr value (tsto, vsto) =
    let vsto = VSto.add addr value vsto in
    (tsto, vsto)

  let fresh (tsto, vsto) =
    let fresh_tsto = TSto.fresh tsto in
    let fresh_vsto = VSto.fresh vsto in
    if fresh_tsto > fresh_vsto then fresh_tsto else fresh_vsto

  let pp fmt (tsto, vsto) =
    Format.fprintf fmt "%a, %a" TSto.pp tsto VSto.pp vsto
end

let find_var (name : Ds.Var.t) ((genv, lenv, sto) : Env.t * Env.t * Sto.t) : Type.t * Value.t =
  let addr = try Env.find name lenv with Not_found -> Env.find name genv in
  Sto.find addr sto

let find_var_global (name : Ds.Var.t) ((genv, _, sto) : Env.t * Env.t * Sto.t) : Type.t * Value.t =
  let addr = Env.find name genv in
  Sto.find addr sto

let add_var (name : Ds.Var.t) (typ : Type.t) (value : Value.t) (env : Env.t) (sto : Sto.t) : Env.t * Sto.t =
  let addr = Sto.fresh sto in
  let env = Env.add name addr env in
  let sto = Sto.add addr typ (Some value) sto in
  (env, sto)

let add_var_without_value (name : Ds.Var.t) (typ : Type.t) (env : Env.t) (sto : Sto.t) : Env.t * Sto.t =
  let addr = Sto.fresh sto in
  let env = Env.add name addr env in
  let sto = Sto.add addr typ None sto in
  (env, sto)

let update_value (name : Ds.Var.t) (value : Value.t) ((genv, lenv, sto) : Env.t * Env.t * Sto.t) =
  let addr = Env.find name lenv in 
  let sto = Sto.update addr value sto in
  (genv, lenv, sto)
