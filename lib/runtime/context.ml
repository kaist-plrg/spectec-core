open Domain
open Base

(* ctx for instantiation:
   The instantiation does not look into method/function body *)

module ICtx = struct
  (* vis_* always contains all names in env_*
     object-local variables (including "apply" params) are
     preferrably added only to vis_*, but to remember their types
     (since their values should not known at compile time)
     they are also added to env_* with default value *)
  type t = { env_glob : env; vis_glob : vis; env_obj : env; vis_obj : vis }

  let empty =
    {
      env_glob = env_empty;
      vis_glob = vis_empty;
      env_obj = env_empty;
      vis_obj = vis_empty;
    }

  let init env_glob env_obj =
    let vis_glob = env_to_vis env_glob in
    let vis_obj = env_to_vis env_obj in
    { env_glob; vis_glob; env_obj; vis_obj }

  (* Adders and updaters *)

  let add_td_glob name typ ctx =
    let gtdenv, genv, gfenv = ctx.env_glob in
    let gtdenv = TDEnv.add name typ gtdenv in
    let gtdvis, gvis, gfvis = ctx.vis_glob in
    let gtdvis = TDVis.add name gtdvis in
    {
      ctx with
      env_glob = (gtdenv, genv, gfenv);
      vis_glob = (gtdvis, gvis, gfvis);
    }

  let add_td_obj name typ ctx =
    let otdenv, oenv, ofenv = ctx.env_obj in
    let otdenv = TDEnv.add name typ otdenv in
    let otdvis, ovis, ofvis = ctx.vis_obj in
    let otdvis = TDVis.add name otdvis in
    {
      ctx with
      env_obj = (otdenv, oenv, ofenv);
      vis_obj = (otdvis, ovis, ofvis);
    }

  let add_var_glob name typ value ctx =
    let gtdenv, genv, gfenv = ctx.env_glob in
    let genv = Env.add name (typ, value) genv in
    let gtdvis, gvis, gfvis = ctx.vis_glob in
    let gvis = Vis.add name gvis in
    {
      ctx with
      env_glob = (gtdenv, genv, gfenv);
      vis_glob = (gtdvis, gvis, gfvis);
    }

  let add_var_obj name typ value ctx =
    let otdenv, oenv, ofenv = ctx.env_obj in
    let oenv = Env.add name (typ, value) oenv in
    let otdvis, ovis, ofvis = ctx.vis_obj in
    let ovis = Vis.add name ovis in
    {
      ctx with
      env_obj = (otdenv, oenv, ofenv);
      vis_obj = (otdvis, ovis, ofvis);
    }

  let add_func_glob name func ctx =
    let gtdenv, genv, gfenv = ctx.env_glob in
    let gfenv = FEnv.add name func gfenv in
    let gtdvis, gvis, gfvis = ctx.vis_glob in
    let gfvis = FVis.add name gfvis in
    {
      ctx with
      env_glob = (gtdenv, genv, gfenv);
      vis_glob = (gtdvis, gvis, gfvis);
    }

  let add_func_obj name func ctx =
    let otdenv, oenv, ofenv = ctx.env_obj in
    let ofenv = FEnv.add name func ofenv in
    let otdvis, ovis, ofvis = ctx.vis_obj in
    let ofvis = FVis.add name ofvis in
    {
      ctx with
      env_obj = (otdenv, oenv, ofenv);
      vis_obj = (otdvis, ovis, ofvis);
    }

  (* Finders *)

  let find finder name ctx = function
    | Some value -> Some value
    | None -> finder name ctx

  let find_td_glob name ctx =
    let gtdenv, _, _ = env_from_vis ctx.env_glob ctx.vis_glob in
    TDEnv.find name gtdenv

  let find_td_obj name ctx =
    let otdenv, _, _ = env_from_vis ctx.env_obj ctx.vis_obj in
    TDEnv.find name otdenv

  let find_td name ctx = find_td_obj name ctx |> find find_td_glob name ctx

  let find_var_glob name ctx =
    let _, genv, _ = env_from_vis ctx.env_glob ctx.vis_glob in
    Env.find name genv

  let find_var_obj name ctx =
    let _, oenv, _ = env_from_vis ctx.env_obj ctx.vis_obj in
    Env.find name oenv

  let find_var name ctx = find_var_obj name ctx |> find find_var_glob name ctx

  (* Pretty-printers *)

  let pp_vis fmt ctx =
    let gtdvis, gvis, gfvis = ctx.vis_glob in
    let otdvis, ovis, ofvis = ctx.vis_obj in
    Format.fprintf fmt
      "{@;\
       <1 2>@[<v 0>global = %a;@ global-func = %a; global-td = %a;@ object = \
       %a@ object-func = %a;@ object-td = %a@]@;\
       <1 -2>}" Vis.pp gvis FVis.pp gfvis TDVis.pp gtdvis Vis.pp ovis FVis.pp
      ofvis TDVis.pp otdvis

  let pp fmt ctx =
    let gtdenv, genv, gfenv = env_from_vis ctx.env_glob ctx.vis_glob in
    let otdenv, oenv, ofenv = env_from_vis ctx.env_obj ctx.vis_obj in
    Format.fprintf fmt
      "{@;\
       <1 2>@[<v 0>global = %a;@ global-func = %a; global-td = %a;@ object = \
       %a@ object-func = %a;@ object-td = %a@]@;\
       <1 -2>}" Env.pp genv FEnv.pp gfenv TDEnv.pp gtdenv Env.pp oenv FEnv.pp
      ofenv TDEnv.pp otdenv
end

(* ctx for interpretation *)

module Ctx = struct
  type t = {
    id : Path.t * Var.t;
    env_glob : env;
    vis_glob : vis;
    env_obj : env;
    vis_obj : vis;
    env_loc : env_stack;
  }

  let empty =
    {
      id = ([], "");
      env_glob = env_empty;
      vis_glob = vis_empty;
      env_obj = env_empty;
      vis_obj = vis_empty;
      env_loc = env_stack_empty;
    }

  let init id env_glob env_obj env_loc =
    let vis_glob = env_to_vis env_glob in
    let vis_obj = env_to_vis env_obj in
    { id; env_glob; vis_glob; env_obj; vis_obj; env_loc }

  (* Adders and updaters *)

  let add_td_obj name typ ctx =
    let otdenv, oenv, ofenv = ctx.env_obj in
    let otdenv = TDEnv.add name typ otdenv in
    let otdvis, ovis, ofvis = ctx.vis_obj in
    let otdvis = TDVis.add name otdvis in
    {
      ctx with
      env_obj = (otdenv, oenv, ofenv);
      vis_obj = (otdvis, ovis, ofvis);
    }

  let add_td_loc name typ ctx =
    let ltdenv, lenvs = ctx.env_loc in
    let ltdenv = TDEnv.add name typ ltdenv in
    { ctx with env_loc = (ltdenv, lenvs) }

  let add_var_obj name typ value ctx =
    let otdenv, oenv, ofenv = ctx.env_obj in
    let oenv = Env.add name (typ, value) oenv in
    let otdvis, ovis, ofvis = ctx.vis_obj in
    let ovis = Vis.add name ovis in
    {
      ctx with
      env_obj = (otdenv, oenv, ofenv);
      vis_obj = (otdvis, ovis, ofvis);
    }

  let add_var_loc name typ value ctx =
    let ltdenv, lenvs = ctx.env_loc in
    let lenvs =
      match lenvs with
      | [] -> failwith "Frame underflow"
      | env :: rest -> Env.add name (typ, value) env :: rest
    in
    { ctx with env_loc = (ltdenv, lenvs) }

  let add_func_obj name func ctx =
    let otdenv, oenv, ofenv = ctx.env_obj in
    let ofenv = FEnv.add name func ofenv in
    let otdvis, ovis, ofvis = ctx.vis_obj in
    let ofvis = FVis.add name ofvis in
    {
      ctx with
      env_obj = (otdenv, oenv, ofenv);
      vis_obj = (otdvis, ovis, ofvis);
    }

  let update_var name typ value ctx =
    let id = ctx.id in
    let gtdenv, genv, gfenv = ctx.env_glob in
    let _, gvis, _ = ctx.vis_glob in
    let otdenv, oenv, ofenv = ctx.env_obj in
    let _, ovis, _ = ctx.vis_obj in
    let ltdenv, lenvs = ctx.env_loc in
    let rec update_var' name typ value = function
      | [] -> Format.sprintf "Variable %s not found" name |> failwith
      | (env, vis) :: rest -> (
          match Vis.find name vis with
          | Some _ ->
              let env = Env.add name (typ, value) env in
              (env, vis) :: rest
          | None -> (env, vis) :: update_var' name typ value rest)
    in
    let envs =
      List.map
        (fun lenv ->
          let lvis =
            Env.fold (fun var _ vis -> Vis.add var vis) lenv Vis.empty
          in
          (lenv, lvis))
        lenvs
      @ [ (oenv, ovis); (genv, gvis) ]
      |> update_var' name typ value |> List.map fst
    in
    let genv, oenv, lenvs =
      match List.rev envs with
      | genv :: oenv :: lenvs -> (genv, oenv, List.rev lenvs)
      | _ -> assert false
    in
    {
      id;
      env_glob = (gtdenv, genv, gfenv);
      vis_glob = ctx.vis_glob;
      env_obj = (otdenv, oenv, ofenv);
      vis_obj = ctx.vis_obj;
      env_loc = (ltdenv, lenvs);
    }

  (* Finders *)

  let find finder var ctx = function
    | Some value -> Some value
    | None -> finder var ctx

  let find_td_glob tvar ctx =
    let gtdenv, _, _ = env_from_vis ctx.env_glob ctx.vis_glob in
    TDEnv.find tvar gtdenv

  let find_td_obj tvar ctx =
    let otdenv, _, _ = env_from_vis ctx.env_obj ctx.vis_obj in
    TDEnv.find tvar otdenv

  let find_td_loc tvar ctx =
    let ltdenv, _ = ctx.env_loc in
    TDEnv.find tvar ltdenv

  let find_td tvar ctx =
    find_td_loc tvar ctx |> find find_td_obj tvar ctx
    |> find find_td_glob tvar ctx

  let find_var_glob var ctx =
    let _, genv, _ = env_from_vis ctx.env_glob ctx.vis_glob in
    Env.find var genv

  let find_var_obj var ctx =
    let _, oenv, _ = env_from_vis ctx.env_obj ctx.vis_obj in
    Env.find var oenv

  let find_var_loc var ctx =
    let _, lenvs = ctx.env_loc in
    List.fold_left
      (fun value frame ->
        match value with Some _ -> value | None -> Env.find var frame)
      None lenvs

  let find_var var ctx =
    find_var_loc var ctx |> find find_var_obj var ctx
    |> find find_var_glob var ctx

  (* (TODO) resolve overloaded functions with argument names *)
  let find_func' (fid, args) fenv =
    let arity = List.length args in
    let funcs =
      List.filter
        (fun ((fid', params), _) -> fid = fid' && arity = List.length params)
        (FEnv.bindings fenv)
    in
    assert (List.length funcs <= 1);
    match funcs with [] -> None | _ -> Some (List.hd funcs |> snd)

  let find_func_glob (fid, args) ctx =
    let _, _, gfenv = env_from_vis ctx.env_glob ctx.vis_glob in
    find_func' (fid, args) gfenv

  let find_func_obj (fid, args) ctx =
    let _, _, ofenv = env_from_vis ctx.env_obj ctx.vis_obj in
    find_func' (fid, args) ofenv

  let find_func (fid, args) ctx =
    find_func_obj (fid, args) ctx |> find find_func_glob (fid, args) ctx

  (* Frame management *)

  let enter_frame ctx =
    let ltdenv, lenvs = ctx.env_loc in
    let env_loc = (ltdenv, Env.empty :: lenvs) in
    { ctx with env_loc }

  let exit_frame ctx =
    let ltdenv, lenvs = ctx.env_loc in
    let env_loc = (ltdenv, List.tl lenvs) in
    { ctx with env_loc }

  (* Pretty-printers *)

  let pp_vis fmt ctx =
    let gtdvis, gvis, gfvis = ctx.vis_glob in
    let otdvis, ovis, ofvis = ctx.vis_obj in
    Format.fprintf fmt
      "{@;\
       <1 2>@[<v 0>global = %a;@ global-func = %a;@ global-td = %a;@ object = \
       %a;@ object-func = %a;@ object-td = %a@]@;\
       <1 -2>}" Vis.pp gvis FVis.pp gfvis TDVis.pp gtdvis Vis.pp ovis FVis.pp
      ofvis TDVis.pp otdvis

  let pp_var fmt ctx =
    let _, genv, _ = env_from_vis ctx.env_glob ctx.vis_glob in
    let _, oenv, _ = env_from_vis ctx.env_obj ctx.vis_obj in
    let _, lenvs = ctx.env_loc in
    Format.fprintf fmt
      "{@;<1 2>@[<v 0>global = %a;@ object = %a;@ loc = %a@]@;<1 -2>}" Env.pp
      genv Env.pp oenv
      (Format.pp_print_list Env.pp)
      lenvs

  let pp fmt ctx =
    let gtdenv, genv, gfenv = env_from_vis ctx.env_glob ctx.vis_glob in
    let otdenv, oenv, ofenv = env_from_vis ctx.env_obj ctx.vis_obj in
    let ltdenv, lenvs = ctx.env_loc in
    Format.fprintf fmt
      "{@;\
       <1 2>@[<v 0>global = %a;@ global-func = %a;@ global-td = %a;@ object = \
       %a;@ object-func = %a;@ object-td = %a;@ loc = %a;@ loc-td = %a@]@;\
       <1 -2>}" Env.pp genv FEnv.pp gfenv TDEnv.pp gtdenv Env.pp oenv FEnv.pp
      ofenv TDEnv.pp otdenv
      (Format.pp_print_list Env.pp)
      lenvs TDEnv.pp ltdenv
end
