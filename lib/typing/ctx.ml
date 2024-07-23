open Runtime.Vis
open Runtime.Env

type env = {
  (* Typedefs, Functions, Constants, and Variable Types *)
  glob : TDEnv.t * FEnv.t * VEnv.t * TEnv.t;
  obj : TDEnv.t * FEnv.t * VEnv.t * TEnv.t;
  loc : TDEnv.t * (VEnv.t * TEnv.t) list;
}

type vis = {
  glob : TDVis.t * FVis.t * VVis.t * TVis.t;
  obj : TDVis.t * FVis.t * VVis.t * TVis.t;
}

type t = { cc : CCEnv.t; env : env; vis : vis }

let env_from_vis (tdenv, fenv, venv, tenv) (tdvis, fvis, vvis, tvis) =
  let tdenv = TDEnv.filter (fun tvar _ -> TDVis.mem tvar tdvis) tdenv in
  let fenv = FEnv.filter (fun fvar _ -> FVis.mem fvar fvis) fenv in
  let venv = VEnv.filter (fun var _ -> VVis.mem var vvis) venv in
  let tenv = TEnv.filter (fun tvar _ -> TVis.mem tvar tvis) tenv in
  (tdenv, fenv, venv, tenv)

let empty =
  let cc = CCEnv.empty in
  let env =
    {
      glob = (TDEnv.empty, FEnv.empty, VEnv.empty, TEnv.empty);
      obj = (TDEnv.empty, FEnv.empty, VEnv.empty, TEnv.empty);
      loc = (TDEnv.empty, []);
    }
  in
  let vis =
    {
      glob = (TDVis.empty, FVis.empty, VVis.empty, TVis.empty);
      obj = (TDVis.empty, FVis.empty, VVis.empty, TVis.empty);
    }
  in
  { cc; env; vis }

(* Adders *)

let add_td_glob name typ ctx =
  let gtdenv, gfenv, gvenv, gtenv = ctx.env.glob in
  let gtdenv = TDEnv.add name typ gtdenv in
  let gtdvis, gfvis, gvvis, gtvis = ctx.vis.glob in
  let gtdvis = TDVis.add name gtdvis in
  {
    ctx with
    env = { ctx.env with glob = (gtdenv, gfenv, gvenv, gtenv) };
    vis = { ctx.vis with glob = (gtdvis, gfvis, gvvis, gtvis) };
  }

let add_const_glob name value ctx =
  let gtdenv, gfenv, gvenv, gtenv = ctx.env.glob in
  let gvenv = VEnv.add name value gvenv in
  let gtdvis, gfvis, gvvis, gtvis = ctx.vis.glob in
  let gvvis = VVis.add name gvvis in
  {
    ctx with
    env = { ctx.env with glob = (gtdenv, gfenv, gvenv, gtenv) };
    vis = { ctx.vis with glob = (gtdvis, gfvis, gvvis, gtvis) };
  }

let add_type_glob name typ ctx =
  let gtdenv, gfenv, gvenv, gtenv = ctx.env.glob in
  let gtenv = TEnv.add name typ gtenv in
  let gtdvis, gfvis, gvvis, gtvis = ctx.vis.glob in
  let gtvis = TVis.add name gtvis in
  {
    ctx with
    env = { ctx.env with glob = (gtdenv, gfenv, gvenv, gtenv) };
    vis = { ctx.vis with glob = (gtdvis, gfvis, gvvis, gtvis) };
  }

(* Finders *)

let find finder name ctx = function
  | Some value -> Some value
  | None -> finder name ctx

let find_td_glob_opt tvar ctx =
  let gtdenv, _, _, _ = env_from_vis ctx.env.glob ctx.vis.glob in
  TDEnv.find_opt tvar gtdenv

let find_td_glob tvar ctx = find_td_glob_opt tvar ctx |> Option.get

let find_td_obj_opt tvar ctx =
  let otdenv, _, _, _ = env_from_vis ctx.env.obj ctx.vis.obj in
  TDEnv.find_opt tvar otdenv

let find_td_obj tvar ctx = find_td_obj_opt tvar ctx |> Option.get

let find_td_loc_opt tvar ctx =
  let ltdenv, _ = ctx.env.loc in
  TDEnv.find_opt tvar ltdenv

let find_td_loc tvar ctx = find_td_loc_opt tvar ctx |> Option.get

let find_td_opt tvar ctx =
  find_td_loc_opt tvar ctx
  |> find find_td_obj_opt tvar ctx
  |> find find_td_glob_opt tvar ctx

let find_td tvar ctx = find_td_opt tvar ctx |> Option.get

let find_const_glob_opt const ctx =
  let _, _, genv, _ = env_from_vis ctx.env.glob ctx.vis.glob in
  VEnv.find_opt const genv

let find_const_glob const ctx = find_const_glob_opt const ctx |> Option.get

let find_const_obj_opt const ctx =
  let _, _, oenv, _ = env_from_vis ctx.env.obj ctx.vis.obj in
  VEnv.find_opt const oenv

let find_const_obj const ctx = find_const_obj_opt const ctx |> Option.get

let find_const_loc_opt const ctx =
  let _, lenvs = ctx.env.loc in
  let lenvs = List.map fst lenvs in
  List.fold_left
    (fun value frame ->
      match value with Some _ -> value | None -> VEnv.find_opt const frame)
    None lenvs

let find_const_loc const ctx = find_const_loc_opt const ctx |> Option.get

let find_const_opt const ctx =
  find_const_loc_opt const ctx
  |> find find_const_obj_opt const ctx
  |> find find_const_glob_opt const ctx

let find_const const ctx = find_const_opt const ctx |> Option.get

(* Pretty-printer *)

let pp_env fmt (env : env) =
  let gtdenv, gfenv, gvenv, gtenv = env.glob in
  let otdenv, ofenv, oenv, otenv = env.obj in
  let ltdenv, lenvs = env.loc in
  let lvenvs, ltenvs = List.split lenvs in
  Format.fprintf fmt
    "{@;\
     <1 2>@[<v 0>global-td = %a;@ global-func = %a;@ global-const = %a;@ \
     global-type = %a;@ object-td = %a;@ object-func = %a;@ object-const = \
     %a;@ object-type = %a;@ loc-td = %a;@ loc-consts = %a;@ loc-types = %a@]@;\
     <1 -2>}" TDEnv.pp gtdenv FEnv.pp gfenv VEnv.pp gvenv TEnv.pp gtenv TDEnv.pp
    otdenv FEnv.pp ofenv VEnv.pp oenv TEnv.pp otenv TDEnv.pp ltdenv
    (Format.pp_print_list VEnv.pp)
    lvenvs
    (Format.pp_print_list TEnv.pp)
    ltenvs

let pp fmt (ctx : t) =
  Format.fprintf fmt "CCEnv: %a\n" CCEnv.pp ctx.cc;
  Format.fprintf fmt "Env: %a\n" pp_env ctx.env
