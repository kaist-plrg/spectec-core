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

(* Helper to move object-local variable declarations into apply block *)

let var_decl_to_stmt = function
  | VarD { name; init = Some value; _ } -> Some (AssignI (VarE (Bare name), value))
  | _ -> None

(* Loading declarations to environments *)

let load_obj_var (ictx: ICtx.t) (name : Var.t) (typ : typ) =
  (* When using an expression for the size, the expression
     must be parenthesized and compile-time known. (7.1.6.2) *)
  let typ = Eval.eval_type ictx typ in
  let value = Runtime_.Ops.eval_default_value typ in
  ICtx.add_var_obj name typ value ictx

let load_obj_const (ictx: ICtx.t) (name: Var.t) (typ: typ) (value: expr) =
  let typ = Eval.eval_type ictx typ in
  let value = Eval.eval_expr ictx value |> Runtime_.Ops.eval_cast typ in
  ICtx.add_var_obj name typ value ictx 

let load_glob_const (ictx: ICtx.t) (name: Var.t) (typ: typ) (value: expr) =
  let typ = Eval.eval_type ictx typ in
  let value = Eval.eval_expr ictx value |> Runtime_.Ops.eval_cast typ in
  ICtx.add_var_glob name typ value ictx

let load_glob_decl (ccenv: CCEnv.t) (ictx: ICtx.t) (decl: decl) =
  match decl with
  (* Load global const to genv *)
  | ConstD { name; typ; value } ->
      let ictx = load_glob_const ictx name typ value in
      (ccenv, ictx)
  (* Load constructor closures to ccenv *)
  | ParserD { name; tparams; params; cparams; locals; states } ->
      let vis_glob = env_to_vis ictx.glob in
      let cclos = CClos.ParserCC { vis_glob; tparams; params; cparams; locals; states } in
      let ccenv = CCEnv.add name cclos ccenv in
      (ccenv, ictx)
  | ControlD { name; tparams; params; cparams; locals; body } ->
      let vis_glob = env_to_vis ictx.glob in
      let cclos = CClos.ControlCC { vis_glob; tparams; params; cparams; locals; body } in
      let ccenv = CCEnv.add name cclos ccenv in
      (ccenv, ictx)
  (* For package type declaration, also load to tdenv *)
  | PackageTypeD { name; tparams; cparams } ->
      let vis_glob = env_to_vis ictx.glob in
      let cclos = CClos.PackageCC { vis_glob; tparams; cparams } in
      let ccenv = CCEnv.add name cclos ccenv in
      let typ = Type.RefT in
      let ictx = ICtx.add_td_glob name typ ictx in
      (ccenv, ictx)
  (* For extern object declaration, also load to tdenv *)
  | ExternObjectD { name; tparams; methods } ->
      let vis_glob = env_to_vis ictx.glob in
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
      let ictx = ICtx.add_td_glob name typ ictx in
      (ccenv, ictx)
  (* Load types to tdenv *)
  | StructD { name; fields } ->
      let fields = List.map (fun (name, typ) -> (name, Eval.eval_type ictx typ)) fields in
      let typ = Type.StructT fields in
      let ictx = ICtx.add_td_glob name typ ictx in
      (ccenv, ictx)
  | HeaderD { name; fields } ->
      let fields = List.map (fun (name, typ) -> (name, Eval.eval_type ictx typ)) fields in
      let typ = Type.HeaderT fields in
      let ictx = ICtx.add_td_glob name typ ictx in
      (ccenv, ictx)
  | UnionD { name; fields } ->
      let fields = List.map (fun (name, typ) -> (name, Eval.eval_type ictx typ)) fields in
      let typ = Type.UnionT fields in
      let ictx = ICtx.add_td_glob name typ ictx in
      (ccenv, ictx)
  | EnumD { name; members } ->
      let typ = Type.EnumT members in
      let ictx = ICtx.add_td_glob name typ ictx in
      (ccenv, ictx)
  | SEnumD _ ->
      Printf.printf "(TODO: load_glob_decl) Load serializable enum\n";
      (ccenv, ictx)
  | NewTypeD { name; typ; decl }
  | TypeDefD { name; typ; decl } -> (
      match typ, decl with
      | Some typ, None ->
          let typ = Eval.eval_type ictx typ in
          let ictx = ICtx.add_td_glob name typ ictx in
          (ccenv, ictx)
      | None, Some _ ->
          Printf.printf "(TODO: load_glob_decl) Load typedef with decl\n";
          (ccenv, ictx)
      | _ -> assert false)
  | ParserTypeD { name; _ }
  | ControlTypeD { name; _ } ->
      let typ = Type.RefT in
      let ictx = ICtx.add_td_glob name typ ictx in
      (ccenv, ictx)
  | _ ->
      Printf.printf "(TODO: load_glob_decl) Load declaration\n";
      (ccenv, ictx)

(* Instantiating objects *)

let rec eval_targs (ictx_caller: ICtx.t) (ictx_callee: ICtx.t) (tparams : string list) (typs : typ list) =
  assert (List.length tparams = List.length typs);
  List.fold_left2
    (fun ictx_callee tparam typ ->
      let typ = Eval.eval_type ictx_caller typ in
      ICtx.add_td_obj tparam typ ictx_callee)
    ictx_callee tparams typs

and eval_expr (ccenv: CCEnv.t) (ictx: ICtx.t) (path : Path.t) (expr : expr) =
  match expr with
  | InstE (typ, args) ->
      let obj = instantiate_from_expr ccenv ictx path typ args in
      let value = Value.RefV path in
      (Some (path, obj), value)
  | _ ->
      let value = Eval.eval_expr ictx expr in
      (None, value)

and eval_args (ccenv : CCEnv.t) (ictx: ICtx.t)
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
        let typ = Eval.eval_type ictx typ in
        let obj, value =
          eval_expr ccenv ictx (path @ [ param ]) arg
        in
        (bindings @ [ (param, value, typ) ], objs @ [ obj ]))
      ([], []) params args
  in
  let objs = List.filter_map (fun obj -> obj) objs in
  (bindings, objs)

and eval_cargs (ccenv: CCEnv.t) (ictx_caller: ICtx.t) (ictx_callee: ICtx.t)
    (sto_callee: Sto.t) (path: Path.t) (cparams: param list) (cargs: arg list) =
  let bindings, objs = eval_args ccenv ictx_caller path cparams cargs in
  let ictx_callee =
    List.fold_left
      (fun ictx_callee (name, value, typ) -> ICtx.add_var_obj name typ value ictx_callee)
      ictx_callee bindings
  in
  let sto_callee =
    List.fold_left
      (fun sto_callee (path, obj) -> Sto.add path obj sto_callee)
      sto_callee objs
  in
  (ictx_callee, sto_callee)

and instantiate_from_cclos (ccenv: CCEnv.t) (ictx_caller: ICtx.t) (path: Path.t) (cclos: CClos.t) (targs: typ list) (cargs: arg list) =
  match cclos with
  | ParserCC { vis_glob; tparams; params; cparams; locals; states } ->
      (* Initialize the environment and store for the parser object *)
      let ictx_callee =
        let env_glob = env_from_vis ictx_caller.glob vis_glob in
        let env_obj = (TDEnv.empty, Env.empty, FEnv.empty) in
        ICtx.init env_glob env_obj
      in
      let sto_callee = Sto.empty in
      (* Evaluate type arguments *)
      let ictx_callee = eval_targs ictx_caller ictx_callee tparams targs in
      (* Evaluate constructor arguments *)
      let ictx_callee, sto_callee =
        eval_cargs ccenv ictx_caller ictx_callee sto_callee path cparams cargs
      in
      (* Evaluate locals *)
      let ictx_callee, sto_callee =
        List.fold_left
          (fun (ictx_callee, sto_callee) local ->
            instantiate_parser_obj_decl ccenv ictx_callee sto_callee path local)
          (ictx_callee, sto_callee)
          locals
      in
      (* Build methods out of states *)
      let ictx_callee =
        List.fold_left
          (fun (ictx_callee: ICtx.t) (name, body) ->
            (* (TODO) Ignore direct application for now *)
            let vis_obj = env_to_vis ictx_callee.obj in
            let func = Func.StateF { vis_obj; body; } in
            ICtx.add_func_obj name func ictx_callee)
          ictx_callee states
      in
      (* Build "apply" method *)
      let apply = 
        let vis_obj = env_to_vis ictx_callee.obj in
        (* Move object-local variable initializers into apply block *)
        let body_init = List.filter_map var_decl_to_stmt locals in
        (* Transition to the start state *)
        let body = body_init @ [ TransI "start" ] in
        Func.MethodF { vis_obj; tparams = []; params = params; body }
      in
      Object.ParserO {
        vis_glob = env_to_vis ictx_callee.glob;
        env_obj = ictx_callee.obj;
        sto_obj = sto_callee;
        mthd = apply
      }
  | ControlCC { vis_glob; tparams; params; cparams; locals; body } ->
      (* Initialize the environment and store for the control object *)
      let ictx_callee =
        let env_glob = env_from_vis ictx_caller.glob vis_glob in
        let env_obj = (TDEnv.empty, Env.empty, FEnv.empty) in
        ICtx.init env_glob env_obj
      in
      let sto_callee = Sto.empty in
      (* Evaluate type arguments *)
      let ictx_callee = eval_targs ictx_caller ictx_callee tparams targs in
      (* Evaluate constructor arguments *)
      let ictx_callee, sto_callee =
        eval_cargs ccenv ictx_caller ictx_callee sto_callee path cparams cargs
      in
      (* Evaluate locals *)
      let ictx_callee, sto_callee =
        List.fold_left
          (fun (ictx_callee, sto_callee) local ->
            instantiate_control_obj_decl ccenv ictx_callee sto_callee path local)
          (ictx_callee, sto_callee)
          locals
      in
      (* Build "apply" method *)
      let apply = 
        let vis_obj = env_to_vis ictx_callee.obj in
        (* Move object-local variable initializers into apply block *)
        let body_init = List.filter_map var_decl_to_stmt locals in
        (* Transition to the start state *)
        let body = body_init @ [ (BlockI body) ] in
        Func.MethodF { vis_obj; tparams = []; params = params; body }
      in
      Object.ControlO {
        vis_glob = env_to_vis ictx_callee.glob;
        env_obj = ictx_callee.obj;
        sto_obj = sto_callee;
        mthd = apply
      }
  | PackageCC { vis_glob; cparams; _ } ->
      (* Initialize the environment and store for the parser object *)
      let ictx_callee =
        let env_glob = env_from_vis ictx_caller.glob vis_glob in
        let env_obj = (TDEnv.empty, Env.empty, FEnv.empty) in
        ICtx.init env_glob env_obj
      in
      let sto_callee = Sto.empty in
      (* Evaluate constructor arguments *)
      let _ictx_callee, sto_callee =
        eval_cargs ccenv ictx_caller ictx_callee sto_callee path cparams cargs
      in
      Object.PackageO { sto_obj = sto_callee }
  | _ -> assert false

and instantiate_from_expr (ccenv : CCEnv.t) (ictx: ICtx.t)
    (path : Path.t) (typ : typ) (args : arg list) =
  let cclos, targs = cclos_from_type typ ccenv in
  instantiate_from_cclos ccenv ictx path cclos targs args

and instantiate_from_obj_decl (ccenv: CCEnv.t) (ictx: ICtx.t) (sto: Sto.t) (path: Path.t)
  (name: string) (typ: typ) (args: arg list) =
  let path = path @ [ name ] in
  let cclos, targs = cclos_from_type typ ccenv in
  let obj = instantiate_from_cclos ccenv ictx path cclos targs args in
  let value = Value.RefV path in
  let typ = Type.RefT in
  let ictx = ICtx.add_var_obj name typ value ictx in
  let sto = Sto.add path obj sto in
  (ictx, sto)

and instantiate_from_glob_decl (ccenv: CCEnv.t) (ictx: ICtx.t) (sto: Sto.t) (path: Path.t)
  (name: string) (typ: typ) (args: arg list) =
  let path = path @ [ name ] in
  let cclos, targs = cclos_from_type typ ccenv in
  let obj = instantiate_from_cclos ccenv ictx path cclos targs args in
  let value = Value.RefV path in
  let typ = Type.RefT in
  let ictx = ICtx.add_var_glob name typ value ictx in
  let sto = Sto.add path obj sto in
  (ictx, sto)

(* Instantiation or load *)

(* parserLocalElement
   : constantDeclaration | instantiation
   | variableDeclaration | valueSetDeclaration; (13.2) *)
and instantiate_parser_obj_decl
    (ccenv: CCEnv.t) (ictx: ICtx.t)
    (sto: Sto.t) (path : Path.t) (decl : decl) =
  match decl with
  | InstD { name; typ; args; _ } ->
      instantiate_from_obj_decl ccenv ictx sto path name typ args
  | ConstD { name; typ; value } ->
      let ictx = load_obj_const ictx name typ value in
      (ictx, sto)
  | VarD { name; typ; _ } ->
      let ictx = load_obj_var ictx name typ in
      (ictx, sto)
  (* There is no syntax for specifying parameters that are value-sets
     (Appendix F) *)
  | ValueSetD { name; _ } ->
      let path = path @ [ name ] in
      (* (TODO) What should be the runtime representation of value set? *)
      let obj = Object.ValueSetO in
      let value = Value.RefV path in
      let typ = Type.RefT in
      let ictx = ICtx.add_var_obj name typ value ictx in
      let sto = Sto.add path obj sto in
      (ictx, sto)
  | _ -> failwith "(instantiate_parser_obj_decl) Unexpected declaration."

(* controlLocalDeclaration
   : constantDeclaration | actionDeclaration
   | tableDeclaration | instantiation
   | variableDeclaration; (14) *)
and instantiate_control_obj_decl
    (ccenv: CCEnv.t) (ictx: ICtx.t)
    (sto: Sto.t) (path : Path.t) (decl : decl) =
  match decl with
  | InstD { name; typ; args; _ } ->
      instantiate_from_obj_decl ccenv ictx sto path name typ args
  | ConstD { name; typ; value } ->
      let ictx = load_obj_const ictx name typ value in
      (ictx, sto)
  | VarD { name; typ; _ } ->
      let ictx = load_obj_var ictx name typ in
      (ictx, sto)
  | ActionD { name; params; body } ->
      let func =
        Func.ActionF {
          vis_obj = env_to_vis ictx.obj;
          params = params;
          body = body;
        }
      in
      let ictx = ICtx.add_func_obj name func ictx in
      (ictx, sto)
  (* Each table evaluates to a table instance (18.2) *)
  (* There is no syntax for specifying parameters that are tables
     Tables are only intended to be used from within the control
     where they are defined (Appendix F) *)
  | TableD { name; key; actions; entries; default; custom } ->
      let path = path @ [ name ] in
      (* Build a dummy "apply" method for table *)
      let apply = Func.TableF { vis_obj = env_to_vis ictx.obj } in
      let obj = Object.TableO { key; actions; entries; default; custom; mthd = apply } in
      let value = Value.RefV path in
      let typ = Type.RefT in
      let ictx = ICtx.add_var_obj name typ value ictx in
      let sto = Sto.add path obj sto in
      (ictx, sto)
  | _ -> failwith "(instantiate_control_obj_decl) Unexpected declaration."

let instantiate_glob_decl (ccenv: CCEnv.t) (ictx: ICtx.t) (sto: Sto.t) (path: Path.t) (decl: decl) =
  match decl with
  (* Explicit instantiation of a package *)
  | InstD { name; typ; args; _ } ->
      let ictx, sto =
        instantiate_from_glob_decl ccenv ictx sto path name typ args
      in
      (ccenv, ictx, sto)
  (* Load declaration to environments *)
  | _ ->
      let ccenv, ictx = load_glob_decl ccenv ictx decl in
      (ccenv, ictx, sto)

let instantiate_program (program: program) =
  let ccenv = CCEnv.empty in
  let ictx = ICtx.empty in
  let sto = Sto.empty in
  let ccenv, ictx, sto =
    List.fold_left
      (fun (ccenv, ictx, sto) decl ->
        instantiate_glob_decl ccenv ictx sto [] decl)
      (ccenv, ictx, sto) program
  in
  Format.printf "ICtx: %a@." ICtx.pp ictx;
  Format.printf "Sto: %a@." Sto.pp sto;
  (ccenv, ictx, sto)
