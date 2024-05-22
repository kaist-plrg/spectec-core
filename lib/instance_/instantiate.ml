open Syntax.Ast
open Runtime_.Domain
open Runtime_.Base
open Runtime_.Cclos
open Runtime_.Object
open Runtime_.Context

(* Helpers to handle generic types without type inference at the moment *)

let cclos_from_type (typ: typ) (ccenv: CCEnv.t) =
  match typ with
  | NameT (Top name)
  | NameT (Bare name) ->
      let cclos = CCEnv.find name ccenv |> Option.get in
      (cclos, [])
  | SpecT (Top name, targs)
  | SpecT (Bare name, targs) ->
      let cclos = CCEnv.find name ccenv |> Option.get in
      (cclos, targs)
  | _ -> assert false

(* Helpers to handle checks that should have been done in type checker *)

(* It is illegal to use names only for some arguments:
   either all or no arguments must specify the parameter name. (8.20) *)
let check_args (args : arg list) =
  assert (
    List.for_all
      (fun (arg : arg) ->
        match arg with ExprA _ -> true | _ -> false)
      args
    || List.for_all
         (fun (arg : arg) ->
           match arg with NameA _ -> true | _ -> false)
         args)

(* Environment and visibility management *)

let env_to_vis (env: TDEnv.t * Env.t * FEnv.t) =
  let tdenv, env, fenv = env in
  let tdvis = TDEnv.fold (fun name _ vis -> TDVis.add name vis) tdenv TDVis.empty in
  let vis = Env.fold (fun var _ vis -> Vis.add var vis) env Vis.empty in
  let fvis = FEnv.fold (fun fname _ vis -> FVis.add fname vis) fenv FVis.empty in
  (tdvis, vis, fvis)

let env_from_vis (env: TDEnv.t * Env.t * FEnv.t) (vis: TDVis.t * Vis.t * FVis.t) =
  let tdenv, env, fenv = env in
  let tdvis, vis, fvis = vis in
  let tdenv = TDEnv.filter (fun name _ -> TDVis.mem name tdvis) tdenv in
  let env = Env.filter (fun var _ -> Vis.mem var vis) env in
  let fenv = FEnv.filter (fun fname _ -> FVis.mem fname fvis) fenv in
  (tdenv, env, fenv)

let add_env (name: Var.t) (typ: Type.t) (value: Value.t) (env: TDEnv.t * Env.t * FEnv.t) =
  let tdenv, env, fenv = env in
  let env = Env.add name (typ, value) env in
  (tdenv, env, fenv)

let add_tdenv (name: Var.t) (typ: Type.t) (env: TDEnv.t * Env.t * FEnv.t) =
  let tdenv, env, fenv = env in
  let tdenv = TDEnv.add name typ tdenv in
  (tdenv, env, fenv)

(* Loading declarations to environments *)

let load_local_variable (env_glob: env_glob) (env_obj: env_obj)
    (name : Var.t) (typ : typ) =
  (* When using an expression for the size, the expression
     must be parenthesized and compile-time known. (7.1.6.2) *)
  let ictx = ICtx.new_obj env_glob env_obj in
  let typ = Eval.eval_type ictx typ in
  let value = Runtime_.Ops.eval_default_value typ in
  add_env name typ value env_obj

let load_local_const (env_glob: env_glob) (env_obj: env_obj) (name: Var.t) (typ: typ) (value: expr) =
  let ictx = ICtx.new_obj env_glob env_obj in
  let typ = Eval.eval_type ictx typ in
  let value = Eval.eval_expr ictx value |> Runtime_.Ops.eval_cast typ in
  add_env name typ value env_obj

let load_glob_const (env_glob: env_glob) (name: Var.t) (typ: typ) (value: expr) =
  let ictx = ICtx.new_glob env_glob in
  let typ = Eval.eval_type ictx typ in
  let value = Eval.eval_expr ictx value |> Runtime_.Ops.eval_cast typ in
  add_env name typ value env_glob

let load_glob_decl (ccenv: CCEnv.t) (env_glob: env_glob) (decl: decl) =
  match decl with
  (* Load global const to genv *)
  | ConstD { name; typ; value } ->
      let env_glob = load_glob_const env_glob name typ value in
      (ccenv, env_glob)
  (* Load constructor closures to ccenv *)
  | ParserD { name; tparams; params; cparams; locals; states } ->
      let vis_glob = env_to_vis env_glob in
      let cclos = CClos.ParserCC { vis_glob; tparams; params; cparams; locals; states } in
      let ccenv = CCEnv.add name cclos ccenv in
      (ccenv, env_glob)
  | ControlD { name; tparams; params; cparams; locals; body } ->
      let vis_glob = env_to_vis env_glob in
      let cclos = CClos.ControlCC { vis_glob; tparams; params; cparams; locals; body } in
      let ccenv = CCEnv.add name cclos ccenv in
      (ccenv, env_glob)
  (* For package type declaration, also load to tdenv *)
  | PackageTypeD { name; tparams; cparams } ->
      let vis_glob = env_to_vis env_glob in
      let cclos = CClos.PackageCC { vis_glob; tparams; cparams } in
      let ccenv = CCEnv.add name cclos ccenv in
      let typ = Type.RefT in
      let env_glob = add_tdenv name typ env_glob in
      (ccenv, env_glob)
  (* For extern object declaration, also load to tdenv *)
  | ExternObjectD { name; tparams; methods } ->
      let vis_glob = env_to_vis env_glob in
      let cons, methods =
        List.partition
          (fun (mthd: decl) -> match mthd with ConsD _ -> true | _ -> false)
          methods
      in
      (* (TODO) Handle overloaded constructors *)
      assert (List.length cons <= 1);
      let cparams =
        match cons with ConsD { cparams; _ } :: [] -> cparams | _ -> []
      in
      let cclos = CClos.ExternCC { vis_glob; tparams; cparams; methods } in
      let ccenv = CCEnv.add name cclos ccenv in
      let typ = Type.RefT in
      let env_glob = add_tdenv name typ env_glob in
      (ccenv, env_glob)
  (* Load types to tdenv *)
  | StructD { name; fields } ->
      let ictx = ICtx.new_glob env_glob in
      let fields = List.map (fun (name, typ) -> (name, Eval.eval_type ictx typ)) fields in
      let typ = Type.StructT fields in
      let env_glob = add_tdenv name typ env_glob in
      (ccenv, env_glob)
  | HeaderD { name; fields } ->
      let ictx = ICtx.new_glob env_glob in
      let fields = List.map (fun (name, typ) -> (name, Eval.eval_type ictx typ)) fields in
      let typ = Type.HeaderT fields in
      let env_glob = add_tdenv name typ env_glob in
      (ccenv, env_glob)
  | UnionD { name; fields } ->
      let ictx = ICtx.new_glob env_glob in
      let fields = List.map (fun (name, typ) -> (name, Eval.eval_type ictx typ)) fields in
      let typ = Type.UnionT fields in
      let env_glob = add_tdenv name typ env_glob in
      (ccenv, env_glob)
  | EnumD { name; members } ->
      let typ = Type.EnumT members in
      let env_glob = add_tdenv name typ env_glob in
      (ccenv, env_glob)
  | SEnumD _ ->
      Printf.printf "(TODO: load_glob_decl) Load serializable enum\n";
      (ccenv, env_glob)
  | NewTypeD { name; typ; decl }
  | TypeDefD { name; typ; decl } -> (
      match typ, decl with
      | Some typ, None ->
          let ictx = ICtx.new_glob env_glob in
          let typ = Eval.eval_type ictx typ in
          let env_glob = add_tdenv name typ env_glob in
          (ccenv, env_glob)
      | None, Some _ ->
          Printf.printf "(TODO: load_glob_decl) Load typedef with decl\n";
          (ccenv, env_glob)
      | _ -> assert false)
  | ParserTypeD { name; _ }
  | ControlTypeD { name; _ } ->
      let typ = Type.RefT in
      let env_glob = add_tdenv name typ env_glob in
      (ccenv, env_glob)
  | _ ->
      Printf.printf "(TODO: load_glob_decl) Load declaration\n";
      (ccenv, env_glob)

(* Instantiating objects *)

let rec eval_targs (env_glob: env_glob) (env_obj: env_obj) (tparams : string list) (typs : typ list) =
  assert (List.length tparams = List.length typs);
  List.fold_left2
    (fun env_obj tparam typ ->
      let ictx = ICtx.new_glob env_glob in
      let typ = Eval.eval_type ictx typ in
      add_tdenv tparam typ env_obj)
    env_obj tparams typs

and eval_expr (ccenv: CCEnv.t) (env_glob: env_glob) (path : Path.t) (expr : expr) =
  match expr with
  | InstE (typ, args) ->
      let obj = instantiate_from_expr ccenv env_glob path typ args in
      let value = Value.RefV path in
      (Some (path, obj), value)
  | _ ->
      let ictx = ICtx.new_glob env_glob in
      let value = Eval.eval_expr ictx expr in
      (None, value)

and eval_args (ccenv : CCEnv.t) (env_glob: env_glob)
    (path : Path.t) (params : param list) (args : arg list) =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  check_args args;
  (* Align by parameter order *)
  let align_args (params, args) (param : param) (arg : arg) =
    let name, _, typ, _ = param in
    match arg with
    | ExprA value ->
        (params @ [ (name, typ) ], args @ [ value ])
    | NameA (name, value) ->
        (params @ [ (name, typ) ], args @ [ value ])
    | _ -> failwith "(eval_args) Instantiation argument must not be missing."
  in
  let params, args = List.fold_left2 align_args ([], []) params args in
  (* Evaluate in order *)
  let bindings, objs =
    List.fold_left2
      (fun (bindings, objs) (param, typ) arg ->
        let ictx = ICtx.new_glob env_glob in
        let typ = Eval.eval_type ictx typ in
        let obj, value =
          eval_expr ccenv env_glob (path @ [ param ]) arg
        in
        (bindings @ [ (param, value, typ) ], objs @ [ obj ]))
      ([], []) params args
  in
  let objs = List.filter_map (fun obj -> obj) objs in
  (bindings, objs)

and eval_cargs (ccenv: CCEnv.t) (env_glob: env_glob) (env_obj: env_obj)
    (sto_obj: Sto.t) (path: Path.t) (cparams: param list) (cargs: arg list) =
  let bindings, objs = eval_args ccenv env_glob path cparams cargs in
  let env_obj =
    List.fold_left
      (fun env_obj (name, value, typ) -> add_env name typ value env_obj)
      env_obj bindings
  in
  let sto_obj =
    List.fold_left
      (fun sto_obj (path, obj) -> Sto.add path obj sto_obj)
      sto_obj objs
  in
  (env_obj, sto_obj)

and instantiate_from_cclos (ccenv: CCEnv.t) (env_glob: env_glob) (path: Path.t) (cclos: CClos.t) (targs: typ list) (cargs: arg list) =
  Format.printf "Instantiate from cclos %a\n" Path.pp path;
  match cclos with
  | ParserCC { vis_glob; tparams; cparams; locals; _ } ->
      (* Initialize the environment and store for the parser object *)
      let env_obj = (TDEnv.empty, Env.empty, FEnv.empty) in
      let sto_obj = Sto.empty in
      (* Evaluate type arguments *)
      let env_obj = eval_targs env_glob env_obj tparams targs in
      (* Evaluate constructor arguments *)
      let env_obj, sto_obj =
        eval_cargs ccenv env_glob env_obj sto_obj path cparams cargs
      in
      (* Evaluation under the caller's visibility is over,
         so restrict the visibility of globals for the parser object *)
      let _env_glob = env_from_vis env_glob vis_glob in
      (* Evaluate locals *)
      let _env_obj, _sto_obj =
        List.fold_left
          (fun (env_obj, sto_obj) local ->
            instantiate_parser_obj_decl ccenv env_glob env_obj sto_obj path local)
          (env_obj, sto_obj)
          locals
      in
      assert false
  | PackageCC { cparams; _ } ->
      (* Initialize the environment and store for the package object *)
      let env_obj = (TDEnv.empty, Env.empty, FEnv.empty) in
      let sto_obj = Sto.empty in
      (* Evaluate constructor arguments *)
      let _env_obj, sto_obj = eval_cargs ccenv env_glob env_obj sto_obj path cparams cargs in
      Object.PackageO { sto_obj }
  | _ -> assert false

and instantiate_from_expr (ccenv : CCEnv.t) (env_glob : env_glob)
    (path : Path.t) (typ : typ) (args : arg list) =
  let cclos, targs = cclos_from_type typ ccenv in
  instantiate_from_cclos ccenv env_glob path cclos targs args

and instantiate_from_obj_decl (ccenv: CCEnv.t) (env_glob: env_glob) (env_obj: env_obj) (path: Path.t)
  (name: string) (typ: typ) (args: arg list) =
  let path = path @ [ name ] in
  let cclos, targs = cclos_from_type typ ccenv in
  let _obj = instantiate_from_cclos ccenv env_glob path cclos targs args in
  let typ = Type.RefT in
  let value = Value.RefV path in
  add_env name typ value env_obj

and instantiate_from_glob_decl (ccenv: CCEnv.t) (env_glob: env_glob) (path: Path.t)
  (name: string) (typ: typ) (args: arg list) =
  let path = path @ [ name ] in
  let cclos, targs = cclos_from_type typ ccenv in
  let _obj = instantiate_from_cclos ccenv env_glob path cclos targs args in
  let typ = Type.RefT in
  let value = Value.RefV path in
  add_env name typ value env_glob

(* Instantiation or load *)

(* parserLocalElement
   : constantDeclaration | instantiation
   | variableDeclaration | valueSetDeclaration; (13.2) *)
and instantiate_parser_obj_decl
    (ccenv: CCEnv.t) (env_glob: env_glob) (env_obj: env_obj)
    (sto_obj: Sto.t) (path : Path.t) (decl : decl) =
  match decl with
  | InstD { name; typ; args; _ } ->
      let env_obj =
        instantiate_from_obj_decl ccenv env_glob env_obj path name typ args
      in
      (env_obj, sto_obj)
  | ConstD { name; typ; value } ->
      let env_obj = load_local_const env_glob env_obj name typ value in
      (env_obj, sto_obj)
  | VarD { name; typ; _ } ->
      let env_obj = load_local_variable env_glob env_obj name typ in
      (env_obj, sto_obj)
  (* There is no syntax for specifying parameters that are value-sets
     (Appendix F) *)
  (*
  | ValueSetD { name; _ } -> failwith "TODO"
  *)
  | _ -> failwith "(instantiate_parser_obj_decl) Unexpected declaration."

let instantiate_glob_decl (ccenv: CCEnv.t) (env_glob : env_glob) (path: Path.t) (decl: decl) =
  match decl with
  (* Explicit instantiation of a package *)
  | InstD { name; typ; args; _ } ->
      let env_glob =
        instantiate_from_glob_decl ccenv env_glob path name typ args
      in
      (ccenv, env_glob)
  (* Load declaration to environments *)
  | _ ->
      load_glob_decl ccenv env_glob decl

let instantiate_program (program: program) =
  let ccenv = CCEnv.empty in
  let env_glob = (TDEnv.empty, Env.empty, FEnv.empty) in
  List.fold_left
    (fun (ccenv, env_glob) decl ->
      instantiate_glob_decl ccenv env_glob [] decl)
    (ccenv, env_glob) program
