open Domain
open Base

(* Context for instantiation:
   Instantiation does not look into method/function body *)

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
    let gtdenv, gfenv, genv = ctx.env_glob in
    let gtdenv = TDEnv.add name typ gtdenv in
    let gtdvis, gfvis, gvis = ctx.vis_glob in
    let gtdvis = TDVis.add name gtdvis in
    {
      ctx with
      env_glob = (gtdenv, gfenv, genv);
      vis_glob = (gtdvis, gfvis, gvis);
    }

  let add_td_obj name typ ctx =
    let otdenv, ofenv, oenv = ctx.env_obj in
    let otdenv = TDEnv.add name typ otdenv in
    let otdvis, ofvis, ovis = ctx.vis_obj in
    let otdvis = TDVis.add name otdvis in
    {
      ctx with
      env_obj = (otdenv, ofenv, oenv);
      vis_obj = (otdvis, ofvis, ovis);
    }

  let add_var_glob name value ctx =
    let gtdenv, gfenv, genv = ctx.env_glob in
    let genv = VEnv.add name value genv in
    let gtdvis, gfvis, gvis = ctx.vis_glob in
    let gvis = VVis.add name gvis in
    {
      ctx with
      env_glob = (gtdenv, gfenv, genv);
      vis_glob = (gtdvis, gfvis, gvis);
    }

  let add_var_obj name value ctx =
    let otdenv, ofenv, oenv = ctx.env_obj in
    let oenv = VEnv.add name value oenv in
    let otdvis, ofvis, ovis = ctx.vis_obj in
    let ovis = VVis.add name ovis in
    {
      ctx with
      env_obj = (otdenv, ofenv, oenv);
      vis_obj = (otdvis, ofvis, ovis);
    }

  let add_func_glob name func ctx =
    let gtdenv, gfenv, genv = ctx.env_glob in
    let gfenv = FEnv.add name func gfenv in
    let gtdvis, gfvis, gvis = ctx.vis_glob in
    let gfvis = FVis.add name gfvis in
    {
      ctx with
      env_glob = (gtdenv, gfenv, genv);
      vis_glob = (gtdvis, gfvis, gvis);
    }

  let add_func_obj name func ctx =
    let otdenv, ofenv, oenv = ctx.env_obj in
    let ofenv = FEnv.add name func ofenv in
    let otdvis, ofvis, ovis = ctx.vis_obj in
    let ofvis = FVis.add name ofvis in
    {
      ctx with
      env_obj = (otdenv, ofenv, oenv);
      vis_obj = (otdvis, ofvis, ovis);
    }

  (* Finders *)

  let find finder name ctx = function
    | Some value -> Some value
    | None -> finder name ctx

  let find_td_glob_opt name ctx =
    let gtdenv, _, _ = env_from_vis ctx.env_glob ctx.vis_glob in
    TDEnv.find_opt name gtdenv

  let find_td_glob name ctx = find_td_glob_opt name ctx |> Option.get

  let find_td_obj_opt name ctx =
    let otdenv, _, _ = env_from_vis ctx.env_obj ctx.vis_obj in
    TDEnv.find_opt name otdenv

  let find_td_obj name ctx = find_td_obj_opt name ctx |> Option.get

  let find_td name ctx =
    find_td_obj_opt name ctx |> find find_td_glob_opt name ctx |> Option.get

  let find_var_glob_opt name ctx =
    let _, _, genv = env_from_vis ctx.env_glob ctx.vis_glob in
    VEnv.find_opt name genv

  let find_var_glob name ctx = find_var_glob_opt name ctx |> Option.get

  let find_var_obj_opt name ctx =
    let _, _, oenv = env_from_vis ctx.env_obj ctx.vis_obj in
    VEnv.find_opt name oenv

  let find_var_obj name ctx = find_var_obj_opt name ctx |> Option.get

  let find_var name ctx =
    find_var_obj_opt name ctx |> find find_var_glob_opt name ctx |> Option.get

  (* Type simplification *)

  let rec simplify_td (typ : Type.t) ctx =
    match typ with
    | NameT name ->
        let typ = find_td name ctx in
        simplify_td typ ctx
    | NewT name -> find_td name ctx
    | _ -> typ

  (* Pretty-printers *)

  let pp_vis fmt ctx =
    let gtdvis, gfvis, gvis = ctx.vis_glob in
    let otdvis, ofvis, ovis = ctx.vis_obj in
    Format.fprintf fmt
      "{@;\
       <1 2>@[<v 0>global = %a;@ global-func = %a; global-td = %a;@ object = \
       %a@ object-func = %a;@ object-td = %a@]@;\
       <1 -2>}" VVis.pp gvis FVis.pp gfvis TDVis.pp gtdvis VVis.pp ovis FVis.pp
      ofvis TDVis.pp otdvis

  let pp fmt ctx =
    let gtdenv, gfenv, genv = env_from_vis ctx.env_glob ctx.vis_glob in
    let otdenv, ofenv, oenv = env_from_vis ctx.env_obj ctx.vis_obj in
    Format.fprintf fmt
      "{@;\
       <1 2>@[<v 0>global = %a;@ global-func = %a; global-td = %a;@ object = \
       %a@ object-func = %a;@ object-td = %a@]@;\
       <1 -2>}" VEnv.pp genv FEnv.pp gfenv TDEnv.pp gtdenv VEnv.pp oenv FEnv.pp
      ofenv TDEnv.pp otdenv
end

(* Context for interpretation *)

module Ctx = struct
  type t = {
    id : Path.t * FId.t;
    env_glob : env;
    vis_glob : vis;
    env_obj : env;
    vis_obj : vis;
    env_loc : env_stack;
  }

  let empty =
    {
      id = ([], ("", []));
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

  let set_id id ctx = { ctx with id }

  let add_td_obj name typ ctx =
    let otdenv, ofenv, oenv = ctx.env_obj in
    let otdenv = TDEnv.add name typ otdenv in
    let otdvis, ofvis, ovis = ctx.vis_obj in
    let otdvis = TDVis.add name otdvis in
    {
      ctx with
      env_obj = (otdenv, ofenv, oenv);
      vis_obj = (otdvis, ofvis, ovis);
    }

  let add_td_loc name typ ctx =
    let ltdenv, lenvs = ctx.env_loc in
    let ltdenv = TDEnv.add name typ ltdenv in
    { ctx with env_loc = (ltdenv, lenvs) }

  let add_var_obj name value ctx =
    let otdenv, ofenv, oenv = ctx.env_obj in
    let oenv = VEnv.add name value oenv in
    let otdvis, ofvis, ovis = ctx.vis_obj in
    let ovis = VVis.add name ovis in
    {
      ctx with
      env_obj = (otdenv, ofenv, oenv);
      vis_obj = (otdvis, ofvis, ovis);
    }

  let add_var_loc name value ctx =
    let ltdenv, lenvs = ctx.env_loc in
    let lenvs =
      match lenvs with
      | [] -> failwith "Frame underflow"
      | env :: rest -> VEnv.add name value env :: rest
    in
    { ctx with env_loc = (ltdenv, lenvs) }

  let add_func_obj name func ctx =
    let otdenv, ofenv, oenv = ctx.env_obj in
    let ofenv = FEnv.add name func ofenv in
    let otdvis, ofvis, ovis = ctx.vis_obj in
    let ofvis = FVis.add name ofvis in
    {
      ctx with
      env_obj = (otdenv, ofenv, oenv);
      vis_obj = (otdvis, ofvis, ovis);
    }

  let update_var name value ctx =
    let id = ctx.id in
    let gtdenv, gfenv, genv = ctx.env_glob in
    let _, _, gvis = ctx.vis_glob in
    let otdenv, ofenv, oenv = ctx.env_obj in
    let _, _, ovis = ctx.vis_obj in
    let ltdenv, lenvs = ctx.env_loc in
    let rec update_var' name value = function
      | [] -> Format.sprintf "Variable %s not found" name |> failwith
      | (env, vis) :: rest -> (
          match VVis.find name vis with
          | Some _ ->
              let env = VEnv.add name value env in
              (env, vis) :: rest
          | None -> (env, vis) :: update_var' name value rest)
    in
    let envs =
      List.map
        (fun lenv ->
          let lvis =
            VEnv.fold (fun var _ vis -> VVis.add var vis) lenv VVis.empty
          in
          (lenv, lvis))
        lenvs
      @ [ (oenv, ovis); (genv, gvis) ]
      |> update_var' name value |> List.map fst
    in
    let genv, oenv, lenvs =
      match List.rev envs with
      | genv :: oenv :: lenvs -> (genv, oenv, List.rev lenvs)
      | _ -> assert false
    in
    {
      id;
      env_glob = (gtdenv, gfenv, genv);
      vis_glob = ctx.vis_glob;
      env_obj = (otdenv, ofenv, oenv);
      vis_obj = ctx.vis_obj;
      env_loc = (ltdenv, lenvs);
    }

  (* Finders *)

  let find finder var ctx = function
    | Some value -> Some value
    | None -> finder var ctx

  let find_td_glob_opt tvar ctx =
    let gtdenv, _, _ = env_from_vis ctx.env_glob ctx.vis_glob in
    TDEnv.find_opt tvar gtdenv

  let find_td_glob tvar ctx = find_td_glob_opt tvar ctx |> Option.get

  let find_td_obj_opt tvar ctx =
    let otdenv, _, _ = env_from_vis ctx.env_obj ctx.vis_obj in
    TDEnv.find_opt tvar otdenv

  let find_td_obj tvar ctx = find_td_obj_opt tvar ctx |> Option.get

  let find_td_loc_opt tvar ctx =
    let ltdenv, _ = ctx.env_loc in
    TDEnv.find_opt tvar ltdenv

  let find_td_loc tvar ctx = find_td_loc_opt tvar ctx |> Option.get

  let find_td tvar ctx =
    find_td_loc_opt tvar ctx
    |> find find_td_obj_opt tvar ctx
    |> find find_td_glob_opt tvar ctx
    |> Option.get

  let find_var_glob_opt var ctx =
    let _, _, genv = env_from_vis ctx.env_glob ctx.vis_glob in
    VEnv.find_opt var genv

  let find_var_glob var ctx = find_var_glob_opt var ctx |> Option.get

  let find_var_obj_opt var ctx =
    let _, _, oenv = env_from_vis ctx.env_obj ctx.vis_obj in
    VEnv.find_opt var oenv

  let find_var_obj var ctx = find_var_obj_opt var ctx |> Option.get

  let find_var_loc_opt var ctx =
    let _, lenvs = ctx.env_loc in
    List.fold_left
      (fun value frame ->
        match value with Some _ -> value | None -> VEnv.find_opt var frame)
      None lenvs

  let find_var_loc var ctx = find_var_loc_opt var ctx |> Option.get

  let find_var var ctx =
    find_var_loc_opt var ctx
    |> find find_var_obj_opt var ctx
    |> find find_var_glob_opt var ctx
    |> Option.get

  let find_func_glob_opt (fid, args) ctx =
    let _, gfenv, _ = env_from_vis ctx.env_glob ctx.vis_glob in
    FEnv.find_opt (fid, args) gfenv

  let find_func_glob (fid, args) ctx =
    find_func_glob_opt (fid, args) ctx |> Option.get

  let find_func_obj_opt (fid, args) ctx =
    let _, ofenv, _ = env_from_vis ctx.env_obj ctx.vis_obj in
    FEnv.find_opt (fid, args) ofenv

  let find_func_obj (fid, args) ctx =
    find_func_obj_opt (fid, args) ctx |> Option.get

  let find_func (fid, args) ctx =
    find_func_obj_opt (fid, args) ctx
    |> find find_func_glob_opt (fid, args) ctx
    |> Option.get

  (* Type simplification *)

  let rec simplify_td (typ : Type.t) ctx =
    match typ with
    | NameT name ->
        let typ = find_td name ctx in
        simplify_td typ ctx
    | NewT name -> find_td name ctx
    | _ -> typ

  (* Frame management *)

  let enter_frame ctx =
    let ltdenv, lenvs = ctx.env_loc in
    let env_loc = (ltdenv, VEnv.empty :: lenvs) in
    { ctx with env_loc }

  let exit_frame ctx =
    let ltdenv, lenvs = ctx.env_loc in
    let env_loc = (ltdenv, List.tl lenvs) in
    { ctx with env_loc }

  (* Pretty-printers *)

  let pp_vis fmt ctx =
    let gtdvis, gfvis, gvis = ctx.vis_glob in
    let otdvis, ofvis, ovis = ctx.vis_obj in
    Format.fprintf fmt
      "{@;\
       <1 2>@[<v 0>global = %a;@ global-func = %a;@ global-td = %a;@ object = \
       %a;@ object-func = %a;@ object-td = %a@]@;\
       <1 -2>}" VVis.pp gvis FVis.pp gfvis TDVis.pp gtdvis VVis.pp ovis FVis.pp
      ofvis TDVis.pp otdvis

  let pp_var fmt ctx =
    let _, _, genv = env_from_vis ctx.env_glob ctx.vis_glob in
    let _, _, oenv = env_from_vis ctx.env_obj ctx.vis_obj in
    let _, lenvs = ctx.env_loc in
    Format.fprintf fmt
      "{@;<1 2>@[<v 0>global = %a;@ object = %a;@ loc = %a@]@;<1 -2>}" VEnv.pp
      genv VEnv.pp oenv
      (Format.pp_print_list VEnv.pp)
      lenvs

  let pp fmt ctx =
    let gtdenv, gfenv, genv = env_from_vis ctx.env_glob ctx.vis_glob in
    let otdenv, ofenv, oenv = env_from_vis ctx.env_obj ctx.vis_obj in
    let ltdenv, lenvs = ctx.env_loc in
    Format.fprintf fmt
      "{@;\
       <1 2>@[<v 0>global = %a;@ global-func = %a;@ global-td = %a;@ object = \
       %a;@ object-func = %a;@ object-td = %a;@ loc = %a;@ loc-td = %a@]@;\
       <1 -2>}" VEnv.pp genv FEnv.pp gfenv TDEnv.pp gtdenv VEnv.pp oenv FEnv.pp
      ofenv TDEnv.pp otdenv
      (Format.pp_print_list VEnv.pp)
      lenvs TDEnv.pp ltdenv
end
