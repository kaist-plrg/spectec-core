open Domain
open Vis

(* Environment of type variables, variables, functions,
   and constructor closures *)

module TDEnv = MakeEnv (Id) (Type)
module TEnv = MakeEnv (Id) (Type)
module VEnv = MakeEnv (Id) (Value)

module FEnv = struct
  include MakeEnv (FId) (Func)

  (* (TODO) resolve overloaded functions with argument names *)
  let find_opt (fid, args) fenv =
    let arity = List.length args in
    let funcs =
      List.filter
        (fun ((fid', params), _) -> fid = fid' && arity = List.length params)
        (bindings fenv)
    in
    assert (List.length funcs <= 1);
    match funcs with [] -> None | _ -> Some (List.hd funcs |> snd)

  let find (fid, args) fenv =
    match find_opt (fid, args) fenv with
    | Some f -> f
    | None -> Format.asprintf "Key not found: %s@." fid |> failwith
end

module CCEnv = struct
  include MakeEnv (FId) (Cclos)

  (* (TODO) resolve overloaded functions with argument names *)
  let find_opt (cid, args) ccenv =
    let arity = List.length args in
    let ccloss =
      List.filter
        (fun ((cid', params), _) -> cid = cid' && arity = List.length params)
        (bindings ccenv)
    in
    assert (List.length ccloss <= 1);
    match ccloss with [] -> None | _ -> Some (List.hd ccloss |> snd)

  let find (cid, args) ccenv =
    match find_opt (cid, args) ccenv with
    | Some c -> c
    | None -> Format.asprintf "Key not found: %s@." cid |> failwith
end

type env = TDEnv.t * FEnv.t * VEnv.t

let env_empty = (TDEnv.empty, FEnv.empty, VEnv.empty)

type env_stack = TDEnv.t * VEnv.t list

let env_stack_empty = (TDEnv.empty, [])

(* Transition between visibility and environment *)

let env_to_vis (env : env) =
  let tdenv, fenv, venv = env in
  let tdvis =
    TDEnv.fold (fun tvar _ tdvis -> TDVis.add tvar tdvis) tdenv TDVis.empty
  in
  let fvis =
    FEnv.fold (fun fvar _ fvis -> FVis.add fvar fvis) fenv FVis.empty
  in
  let vvis = VEnv.fold (fun var _ vvis -> VVis.add var vvis) venv VVis.empty in
  (tdvis, fvis, vvis)

let env_from_vis (env : env) (vis : vis) =
  let tdenv, fenv, venv = env in
  let tdvis, fvis, vvis = vis in
  let tdenv = TDEnv.filter (fun tvar _ -> TDVis.mem tvar tdvis) tdenv in
  let fenv = FEnv.filter (fun fvar _ -> FVis.mem fvar fvis) fenv in
  let venv = VEnv.filter (fun var _ -> VVis.mem var vvis) venv in
  (tdenv, fenv, venv)
