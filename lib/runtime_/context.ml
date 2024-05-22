open Base

(* ctx for instantiation:
   The instantiation does not look into method/function body *)

module ICtx = struct
  type t = {
    glob : env_glob;
    obj : env_obj;
  }

  let empty =
    {
      glob = (TDEnv.empty, Env.empty, FEnv.empty);
      obj = (TDEnv.empty, Env.empty, FEnv.empty);
    }

  let new_glob env_glob =
    { glob = env_glob; obj = (TDEnv.empty, Env.empty, FEnv.empty) }

  let new_obj env_glob env_obj =
    { glob = env_glob; obj = env_obj }

  let find_td name ctx =
    let gtdenv, _, _ = ctx.glob in
    let otdenv, _, _ = ctx.obj in
    List.fold_left
      (fun value tdenv ->
        match value with Some _ -> value | None -> TDEnv.find name tdenv)
      None [ otdenv; gtdenv ]

  let find_td_top name ctx =
    let gtdenv, _, _ = ctx.glob in
    TDEnv.find name gtdenv

  let find_var name ctx =
    let _, genv, _ = ctx.glob in
    let _, oenv, _ = ctx.obj in
    List.fold_left
      (fun value env ->
        match value with Some _ -> value | None -> Env.find name env)
      None [ oenv; genv ]

  let find_var_top name ctx =
    let _, genv, _ = ctx.glob in
    Env.find name genv

  let pp fmt ctx =
    let _, genv, _ = ctx.glob in
    let _, oenv, _ = ctx.obj in
    Format.fprintf fmt "{@[<v 2>global = %a;@ object = %a@]}"
      Env.pp genv Env.pp oenv
end

(* ctx for interpretation *)

module Ctx = struct
  type t = {
    glob : env_glob;
    obj : env_obj;
    loc : env_loc;
  }

  let empty =
    {
      glob = (TDEnv.empty, Env.empty, FEnv.empty);
      obj = (TDEnv.empty, Env.empty, FEnv.empty);
      loc = (TDEnv.empty, []);
    }

  let find_var name ctx =
    let _, genv, _ = ctx.glob in
    let _, oenv, _ = ctx.obj in
    let _, lenvs = ctx.loc in
    List.fold_left
      (fun value frame ->
        match value with Some _ -> value | None -> Env.find name frame)
      None
      (lenvs @ [ oenv; genv ])

  let enter_frame ctx =
    let ltdenv, lenvs = ctx.loc in
    let loc = (ltdenv, Env.empty :: lenvs) in
    { ctx with loc }

  let exit_frame ctx =
    let ltdenv, lenvs = ctx.loc in
    let loc = (ltdenv, List.tl lenvs) in
    { ctx with loc }

  let pp fmt ctx =
    let _, genv, _ = ctx.glob in
    let _, oenv, _ = ctx.obj in
    let _, lenvs = ctx.loc in
    Format.fprintf fmt "{@[<v 2>global = %a;@ object = %a;@ local = %a@]}"
      Env.pp genv Env.pp oenv
      (Format.pp_print_list Env.pp) lenvs
end
