open Base
open Object

(* ctx for instantiation:
   The instantiation does not look into method/function body *)

module ICtx = struct
  type t = { glob : env_glob; obj : env_obj }

  let empty =
    {
      glob = (TDEnv.empty, Env.empty, FEnv.empty);
      obj = (TDEnv.empty, Env.empty, FEnv.empty);
    }

  let init env_glob env_obj = { glob = env_glob; obj = env_obj }

  let add_td_glob name typ ctx =
    let gtdenv, genv, gfenv = ctx.glob in
    let gtdenv = TDEnv.add name typ gtdenv in
    { ctx with glob = (gtdenv, genv, gfenv) }

  let add_td_obj name typ ctx =
    let otdenv, oenv, ofenv = ctx.obj in
    let otdenv = TDEnv.add name typ otdenv in
    { ctx with obj = (otdenv, oenv, ofenv) }

  let add_var_glob name typ value ctx =
    let gtdenv, genv, gfenv = ctx.glob in
    let genv = Env.add name (typ, value) genv in
    { ctx with glob = (gtdenv, genv, gfenv) }

  let add_var_obj name typ value ctx =
    let otdenv, oenv, ofenv = ctx.obj in
    let oenv = Env.add name (typ, value) oenv in
    { ctx with obj = (otdenv, oenv, ofenv) }

  let add_func_glob name func ctx =
    let gtdenv, genv, gfenv = ctx.glob in
    let gfenv = FEnv.add name func gfenv in
    { ctx with glob = (gtdenv, genv, gfenv) }

  let add_func_obj name func ctx =
    let otdenv, oenv, ofenv = ctx.obj in
    let ofenv = FEnv.add name func ofenv in
    { ctx with obj = (otdenv, oenv, ofenv) }

  let find_td name ctx =
    let gtdenv, _, _ = ctx.glob in
    let otdenv, _, _ = ctx.obj in
    List.fold_left
      (fun value tdenv ->
        match value with Some _ -> value | None -> TDEnv.find name tdenv)
      None [ otdenv; gtdenv ]

  let find_td_glob name ctx =
    let gtdenv, _, _ = ctx.glob in
    TDEnv.find name gtdenv

  let find_var name ctx =
    let _, genv, _ = ctx.glob in
    let _, oenv, _ = ctx.obj in
    List.fold_left
      (fun value env ->
        match value with Some _ -> value | None -> Env.find name env)
      None [ oenv; genv ]

  let find_var_glob name ctx =
    let _, genv, _ = ctx.glob in
    Env.find name genv

  let pp fmt ctx =
    let _, genv, gfenv = ctx.glob in
    let _, oenv, ofenv = ctx.obj in
    Format.fprintf fmt
      "{@;\
       <1 2>@[<v 0>global = %a;@ global-func = %a;@ object = %a@ object-func = \
       %a;@]@;\
       <1 -2>}" Env.pp genv FEnv.pp gfenv Env.pp oenv FEnv.pp ofenv
end

(* ctx for interpretation *)

module GCtx = struct
  type t = { glob : env_glob; sto : Sto.t }

  let empty = {
    glob = (TDEnv.empty, Env.empty, FEnv.empty);
    sto = Sto.empty;
  }

  let init env_glob sto = { glob = env_glob; sto }

  let find_obj path ctx = Sto.find path ctx.sto

  let pp fmt ctx =
    let _, genv, gfenv = ctx.glob in
    let sto = ctx.sto in
    Format.fprintf fmt
      "{@;\
       <1 2>@[<v 0>global = %a;@ global-func = %a;@ sto = %a@]@;\
       <1 -2>}" Env.pp genv FEnv.pp gfenv Sto.pp sto
end

module Ctx = struct
  type t = { glob : env_glob; obj : env_obj; loc : env_loc }

  let empty =
    {
      glob = (TDEnv.empty, Env.empty, FEnv.empty);
      obj = (TDEnv.empty, Env.empty, FEnv.empty);
      loc = (TDEnv.empty, []);
    }

  let init env_glob env_obj env_loc =
    { glob = env_glob; obj = env_obj; loc = env_loc }

  let add_td_obj name typ ctx =
    let otdenv, oenv, ofenv = ctx.obj in
    let otdenv = TDEnv.add name typ otdenv in
    { ctx with obj = (otdenv, oenv, ofenv) }

  let add_var_obj name typ value ctx =
    let otdenv, oenv, ofenv = ctx.obj in
    let oenv = Env.add name (typ, value) oenv in
    { ctx with obj = (otdenv, oenv, ofenv) }

  let add_var_loc name typ value ctx =
    let ltdenv, lenvs = ctx.loc in
    let lenvs =
      match lenvs with
      | [] -> failwith "Frame underflow"
      | env :: rest -> Env.add name (typ, value) env :: rest
    in
    { ctx with loc = (ltdenv, lenvs) }

  let add_func_obj name func ctx =
    let otdenv, oenv, ofenv = ctx.obj in
    let ofenv = FEnv.add name func ofenv in
    { ctx with obj = (otdenv, oenv, ofenv) }

  let update_var name typ value ctx =
    let gtdenv, genv, gfenv = ctx.glob in
    let otdenv, oenv, ofenv = ctx.obj in
    let ltdenv, lenvs = ctx.loc in
    let rec update_var' name typ value = function
      | [] -> Format.sprintf "Variable %s not found" name |> failwith
      | env :: rest -> (
          match Env.find name env with
          | Some _ ->
              let env = Env.add name (typ, value) env in
              env :: rest
          | None -> env :: update_var' name typ value rest)
    in
    let envs = update_var' name typ value (lenvs @ [ oenv; genv ]) in
    let genv, oenv, lenvs =
      match List.rev envs with
      | genv :: oenv :: lenvs -> (genv, oenv, List.rev lenvs)
      | _ -> assert false
    in
    {
      glob = (gtdenv, genv, gfenv);
      obj = (otdenv, oenv, ofenv);
      loc = (ltdenv, lenvs);
    }

  let find_td name ctx =
    let gtdenv, _, _ = ctx.glob in
    let otdenv, _, _ = ctx.obj in
    let ltdenv, _ = ctx.loc in
    List.fold_left
      (fun value tdenv ->
        match value with Some _ -> value | None -> TDEnv.find name tdenv)
      None [ ltdenv; otdenv; gtdenv ]

  let find_td_glob name ctx =
    let gtdenv, _, _ = ctx.glob in
    TDEnv.find name gtdenv

  let find_var name ctx =
    let _, genv, _ = ctx.glob in
    let _, oenv, _ = ctx.obj in
    let _, lenvs = ctx.loc in
    List.fold_left
      (fun value frame ->
        match value with Some _ -> value | None -> Env.find name frame)
      None
      (lenvs @ [ oenv; genv ])

  let find_var_glob name ctx =
    let _, genv, _ = ctx.glob in
    Env.find name genv

  let enter_frame ctx =
    let ltdenv, lenvs = ctx.loc in
    let loc = (ltdenv, Env.empty :: lenvs) in
    { ctx with loc }

  let exit_frame ctx =
    let ltdenv, lenvs = ctx.loc in
    let loc = (ltdenv, List.tl lenvs) in
    { ctx with loc }

  let pp fmt ctx =
    let _, genv, gfenv = ctx.glob in
    let _, oenv, ofenv = ctx.obj in
    let _, lenvs = ctx.loc in
    Format.fprintf fmt
      "{@;\
       <1 2>@[<v 0>global = %a;@ global-func = %a;@ object = %a;@ object-func \
       = %a;@ loc = %a@]@;\
       <1 -2>}" Env.pp genv FEnv.pp gfenv Env.pp oenv FEnv.pp ofenv
      (Format.pp_print_list Env.pp)
      lenvs
end
