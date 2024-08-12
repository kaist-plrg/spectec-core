open Syntax.Ast
open Runtime.Domain
open Runtime.Base
open Runtime.Cclos
open Runtime.Object
open Runtime.Context
open Util.Source

(* Helpers to handle generic types without type inference at the moment *)

let cclos_from_type (ccenv : CCEnv.t) (typ : typ) (args : arg list) =
  match typ.it with
  | NameT { it = Top id; _ } | NameT { it = Current id; _ } ->
      let cclos = CCEnv.find (id.it, args) ccenv in
      (cclos, [])
  | SpecT ({ it = Top id; _ }, targs) | SpecT ({ it = Current id; _ }, targs) ->
      let cclos = CCEnv.find (id.it, args) ccenv in
      (cclos, targs)
  | _ -> assert false

(* Helpers to handle checks that should have been done in type checker *)

(* It is illegal to use names only for some arguments:
   either all or no arguments must specify the parameter name. (8.20) *)
let check_args (args : arg list) =
  assert (
    List.for_all
      (fun (arg : arg) -> match arg.it with ExprA _ -> true | _ -> false)
      args
    || List.for_all
         (fun (arg : arg) -> match arg.it with NameA _ -> true | _ -> false)
         args)

(* Helper to move object-local variable declarations into apply block *)

let var_decl_to_stmt (decl : decl) =
  match decl.it with
  | VarD { id; init = Some value; _ } ->
      Some (AssignS (VarE (Current id $ no_info) $ no_info, value) $ decl.at)
  | _ -> None

(* Loading declarations to environments *)

let load_obj_var (ictx : ICtx.t) (name : Id.t) (typ : typ) =
  (* When using an expression for the size, the expression
     must be parenthesized and compile-time known. (7.1.6.2) *)
  let typ = Eval.eval_type ictx typ in
  let value = Runtime.Ops.eval_default_value typ in
  ICtx.add_var_obj name value ictx

let load_obj_const (ictx : ICtx.t) (name : Id.t) (typ : typ) (value : expr) =
  let typ = Eval.eval_type ictx typ in
  let value = Eval.eval_expr ictx value |> Runtime.Ops.eval_cast typ in
  ICtx.add_var_obj name value ictx

let load_glob_const (ictx : ICtx.t) (name : Id.t) (typ : typ) (value : expr) =
  let typ = Eval.eval_type ictx typ in
  let value = Eval.eval_expr ictx value |> Runtime.Ops.eval_cast typ in
  ICtx.add_var_glob name value ictx

let load_glob_decl (ccenv : CCEnv.t) (ictx : ICtx.t) (decl : decl) =
  match decl.it with
  (* Load global const to genv *)
  | ConstD { id; typ; value; _ } ->
      let ictx = load_glob_const ictx id.it typ value in
      (ccenv, ictx)
  (* Load constructor closures to ccenv *)
  | ParserD { id; tparams; params; cparams; locals; states; _ } ->
      let cclos = CClos.ParserCC { tparams; params; cparams; locals; states } in
      let cid = FId.to_fid id cparams in
      let ccenv = CCEnv.add cid cclos ccenv in
      (ccenv, ictx)
  | ControlD { id; tparams; params; cparams; locals; body; _ } ->
      let cclos = CClos.ControlCC { tparams; params; cparams; locals; body } in
      let cid = FId.to_fid id cparams in
      let ccenv = CCEnv.add cid cclos ccenv in
      (ccenv, ictx)
  (* For package type declaration, also load to tdenv *)
  | PackageTypeD { id; tparams; cparams; _ } ->
      let cclos = CClos.PackageCC { tparams; cparams } in
      let cid = FId.to_fid id cparams in
      let ccenv = CCEnv.add cid cclos ccenv in
      let typ = Type.RefT in
      let ictx = ICtx.add_td_glob id.it typ ictx in
      (ccenv, ictx)
  (* For extern object declaration, also load to tdenv *)
  | ExternObjectD { id; tparams; mthds; _ } ->
      let cons, mthds =
        List.partition
          (fun (mthd : decl) ->
            match mthd.it with ConsD _ -> true | _ -> false)
          mthds
      in
      (* (TODO) Is this the right way to handle extern object without a constructor? *)
      let cons =
        if List.length cons = 0 then
          [
            {
              it =
                ConsD
                  {
                    id = { it = id.it; at = no_info };
                    cparams = [];
                    annos = [];
                  };
              at = no_info;
            };
          ]
        else cons
      in
      let ccenv =
        List.fold_left
          (fun ccenv con ->
            let cparams =
              match con.it with
              | ConsD { cparams; _ } -> cparams
              | _ -> assert false
            in
            let cclos = CClos.ExternCC { tparams; cparams; mthds } in
            let cid = FId.to_fid id cparams in
            CCEnv.add cid cclos ccenv)
          ccenv cons
      in
      let typ = Type.RefT in
      let ictx = ICtx.add_td_glob id.it typ ictx in
      (ccenv, ictx)
  (* Load types to tdenv *)
  | ErrD { members } ->
      let members = List.map it members in
      let typ = Type.ErrT members in
      let ictx = ICtx.add_td_glob "error" typ ictx in
      (ccenv, ictx)
  | MatchKindD { members } ->
      let members = List.map it members in
      let typ = Type.MatchKindT members in
      let ictx = ICtx.add_td_glob "match_kind" typ ictx in
      (ccenv, ictx)
  | StructD { id; fields; _ } ->
      let fields =
        List.map
          (fun (member, typ, _) -> (member.it, Eval.eval_type ictx typ))
          fields
      in
      let typ = Type.StructT fields in
      let ictx = ICtx.add_td_glob id.it typ ictx in
      (ccenv, ictx)
  | HeaderD { id; fields; _ } ->
      let fields =
        List.map
          (fun (member, typ, _) -> (member.it, Eval.eval_type ictx typ))
          fields
      in
      let typ = Type.HeaderT fields in
      let ictx = ICtx.add_td_glob id.it typ ictx in
      (ccenv, ictx)
  | UnionD { id; fields; _ } ->
      let fields =
        List.map
          (fun (member, typ, _) -> (member.it, Eval.eval_type ictx typ))
          fields
      in
      let typ = Type.UnionT fields in
      let ictx = ICtx.add_td_glob id.it typ ictx in
      (ccenv, ictx)
  | EnumD { id; members; _ } ->
      let members = List.map it members in
      let typ = Type.EnumT (id.it, members) in
      let ictx = ICtx.add_td_glob id.it typ ictx in
      (ccenv, ictx)
  | SEnumD { id; typ; fields; _ } ->
      let typ = Eval.eval_type ictx typ in
      let fields =
        List.map
          (fun (member, expr) ->
            let value = Eval.eval_expr ictx expr |> Runtime.Ops.eval_cast typ in
            (member.it, value))
          fields
      in
      let typ = Type.SEnumT (id.it, typ, fields) in
      let ictx = ICtx.add_td_glob id.it typ ictx in
      (ccenv, ictx)
  | NewTypeD { id; typ; _ } | TypeDefD { id; typ; _ } -> (
      match typ with
      | Left typ ->
          let typ = Eval.eval_type ictx typ in
          let ictx = ICtx.add_td_glob id.it typ ictx in
          (ccenv, ictx)
      | Right _decl ->
          Format.eprintf
            "(TODO: load_glob_decl) Load typedef/newtype with decl\n";
          (ccenv, ictx))
  | ParserTypeD { id; _ } | ControlTypeD { id; _ } ->
      let typ = Type.RefT in
      let ictx = ICtx.add_td_glob id.it typ ictx in
      (ccenv, ictx)
  (* Load functions to fenv *)
  | ActionD { id; params; body; _ } ->
      let vis_glob = env_to_vis ictx.env_glob in
      let func = Func.ActionF { vis = vis_glob; params; body } in
      let fid = FId.to_fid id params in
      let ictx = ICtx.add_func_glob fid func ictx in
      (ccenv, ictx)
  | ExternFuncD { id; tparams; params; _ } ->
      let vis_glob = env_to_vis ictx.env_glob in
      let func = Func.ExternF { vis_glob; tparams; params } in
      let fid = FId.to_fid id params in
      let ictx = ICtx.add_func_glob fid func ictx in
      (ccenv, ictx)
  | _ ->
      Format.eprintf "(TODO: load_glob_decl) Load declaration %a\n"
        (Syntax.Pp.pp_decl ~level:0)
        decl;
      (ccenv, ictx)

(* Instantiating objects *)

let rec eval_targs (ictx_caller : ICtx.t) (ictx_callee : ICtx.t)
    (tparams : tparam list) (targs : typ list) =
  assert (List.length tparams = List.length targs);
  List.fold_left2
    (fun ictx_callee tparam targ ->
      let typ = Eval.eval_type ictx_caller targ in
      ICtx.add_td_obj tparam.it typ ictx_callee)
    ictx_callee tparams targs

and eval_expr (ccenv : CCEnv.t) (sto : Sto.t) (ictx : ICtx.t) (path : Path.t)
    (expr : expr) =
  match expr.it with
  | InstE (typ, args) ->
      let sto = instantiate_from_expr ccenv sto ictx path typ args in
      let value = Value.RefV path in
      (sto, value)
  | _ ->
      let value = Eval.eval_expr ictx expr in
      (sto, value)

and eval_args (ccenv : CCEnv.t) (sto : Sto.t) (ictx_caller : ICtx.t)
    (ictx_callee : ICtx.t) (path : Path.t) (params : param list)
    (args : arg list) =
  (* (TODO) assume there is no default argument *)
  assert (List.length params = List.length args);
  check_args args;
  (* Align by parameter order *)
  let align_args (params, args) (param : param) (arg : arg) =
    let name, _, _, _, _ = param.it in
    match arg.it with
    | ExprA value -> (params @ [ name ], args @ [ value ])
    | NameA (name, value) -> (params @ [ name ], args @ [ value ])
    | _ -> failwith "(eval_args) Instantiation argument must not be missing."
  in
  let params, args = List.fold_left2 align_args ([], []) params args in
  (* Evaluate in order *)
  let sto, ictx_callee =
    List.fold_left2
      (fun (sto, ictx_callee) param arg ->
        let sto, value =
          eval_expr ccenv sto ictx_caller (path @ [ param.it ]) arg
        in
        let ictx_callee = ICtx.add_var_obj param.it value ictx_callee in
        (sto, ictx_callee))
      (sto, ictx_callee) params args
  in
  (sto, ictx_callee)

and eval_cargs (ccenv : CCEnv.t) (sto : Sto.t) (ictx_caller : ICtx.t)
    (ictx_callee : ICtx.t) (path : Path.t) (cparams : param list)
    (cargs : arg list) =
  eval_args ccenv sto ictx_caller ictx_callee path cparams cargs

and pre_eval_params (ictx : ICtx.t) (params : param list) =
  List.fold_left
    (fun ictx { it = name, _, typ, _, _; _ } -> load_obj_var ictx name.it typ)
    ictx params

and instantiate_from_cclos (ccenv : CCEnv.t) (sto : Sto.t)
    (ictx_caller : ICtx.t) (path : Path.t) (cclos : CClos.t) (targs : typ list)
    (cargs : arg list) =
  match cclos with
  | ParserCC { tparams; params; cparams; locals; states; _ } ->
      (* Initialize the environment for the parser object *)
      let ictx_callee =
        let env_glob = ictx_caller.env_glob in
        let env_obj = env_empty in
        ICtx.init env_glob env_obj
      in
      (* Evaluate type arguments *)
      let ictx_callee = eval_targs ictx_caller ictx_callee tparams targs in
      (* Evaluate constructor arguments *)
      let sto, ictx_callee =
        eval_cargs ccenv sto ictx_caller ictx_callee path cparams cargs
      in
      (* Pre-evaluate parameters *)
      let ictx_callee = pre_eval_params ictx_callee params in
      (* Evaluate locals *)
      let sto, ictx_callee =
        List.fold_left
          (fun (sto, ictx_callee) local ->
            instantiate_parser_obj_decl ccenv sto ictx_callee path local)
          (sto, ictx_callee) locals
      in
      (* Build methods out of states *)
      let ictx_callee =
        List.fold_left
          (fun (ictx_callee : ICtx.t) { it = label, body, _; _ } ->
            (* (TODO) Ignore direct application for now *)
            let func = Func.StateF { body } in
            let fid = (label.it, []) in
            ICtx.add_func_obj fid func ictx_callee)
          ictx_callee states
      in
      (* Build "apply" method *)
      let apply =
        (* Move object-local variable initializers into apply block *)
        let body_init = List.filter_map var_decl_to_stmt locals in
        (* Transition to the start state *)
        let body = body_init @ [ TransS ("start" $ no_info) $ no_info ] in
        let body = (body, []) $ no_info in
        Func.MethodF
          { vis_obj = ictx_callee.vis_obj; tparams = []; params; body }
      in
      let obj =
        Object.ParserO
          {
            vis_glob = ictx_callee.vis_glob;
            env_obj = ictx_callee.env_obj;
            mthd = apply;
          }
      in
      Sto.add path obj sto
  | ControlCC { tparams; params; cparams; locals; body; _ } ->
      (* Initialize the environment for the control object *)
      let ictx_callee =
        let env_glob = ictx_caller.env_glob in
        let env_obj = env_empty in
        ICtx.init env_glob env_obj
      in
      (* Evaluate type arguments *)
      let ictx_callee = eval_targs ictx_caller ictx_callee tparams targs in
      (* Evaluate constructor arguments *)
      let sto, ictx_callee =
        eval_cargs ccenv sto ictx_caller ictx_callee path cparams cargs
      in
      (* Pre-evaluate parameters *)
      let ictx_callee = pre_eval_params ictx_callee params in
      (* Evaluate locals *)
      let sto, ictx_callee =
        List.fold_left
          (fun (sto, ictx_callee) local ->
            instantiate_control_obj_decl ccenv sto ictx_callee path local)
          (sto, ictx_callee) locals
      in
      (* Build "apply" method *)
      let apply =
        (* Move object-local variable initializers into apply block *)
        let body_init = List.filter_map var_decl_to_stmt locals in
        let body = body_init @ [ BlockS body $ no_info ] in
        let body = (body, []) $ no_info in
        Func.MethodF
          { vis_obj = ictx_callee.vis_obj; tparams = []; params; body }
      in
      let obj =
        Object.ControlO
          {
            vis_glob = ictx_callee.vis_glob;
            env_obj = ictx_callee.env_obj;
            mthd = apply;
          }
      in
      Sto.add path obj sto
  | PackageCC { cparams; _ } ->
      (* Initialize the environment and store for the parser object *)
      let ictx_callee =
        let env_glob = ictx_caller.env_glob in
        let env_obj = env_empty in
        ICtx.init env_glob env_obj
      in
      (* Evaluate constructor arguments *)
      let sto, _ictx_callee =
        eval_cargs ccenv sto ictx_caller ictx_callee path cparams cargs
      in
      let obj = Object.PackageO in
      Sto.add path obj sto
  | ExternCC { cparams; mthds; _ } ->
      (* Initialize the environment for the extern object *)
      let ictx_callee =
        let env_glob = ictx_caller.env_glob in
        let env_obj = env_empty in
        ICtx.init env_glob env_obj
      in
      (* Evaluate constructor arguments *)
      let sto, ictx_callee =
        eval_cargs ccenv sto ictx_caller ictx_callee path cparams cargs
      in
      (* Evaluate methods *)
      let ictx_callee =
        List.fold_left instantiate_extern_obj_decl ictx_callee mthds
      in
      let obj =
        Object.ExternO
          {
            vis_glob = env_to_vis ictx_callee.env_glob;
            env_obj = ictx_callee.env_obj;
          }
      in
      Sto.add path obj sto

and instantiate_from_expr (ccenv : CCEnv.t) (sto : Sto.t) (ictx : ICtx.t)
    (path : Path.t) (typ : typ) (args : arg list) =
  let cclos, targs = cclos_from_type ccenv typ args in
  instantiate_from_cclos ccenv sto ictx path cclos targs args

and instantiate_from_obj_decl (ccenv : CCEnv.t) (sto : Sto.t) (ictx : ICtx.t)
    (path : Path.t) (name : Id.t) (typ : typ) (args : arg list) =
  let path = path @ [ name ] in
  let cclos, targs = cclos_from_type ccenv typ args in
  let sto = instantiate_from_cclos ccenv sto ictx path cclos targs args in
  let value = Value.RefV path in
  let ictx = ICtx.add_var_obj name value ictx in
  (sto, ictx)

and instantiate_from_glob_decl (ccenv : CCEnv.t) (sto : Sto.t) (ictx : ICtx.t)
    (name : Id.t) (typ : typ) (args : arg list) =
  let path = [ name ] in
  let cclos, targs = cclos_from_type ccenv typ args in
  let sto = instantiate_from_cclos ccenv sto ictx path cclos targs args in
  let value = Value.RefV path in
  let ictx = ICtx.add_var_glob name value ictx in
  (sto, ictx)

(* Instantiation or load *)

(* parserLocalElement
   : constantDeclaration | instantiation
   | variableDeclaration | valueSetDeclaration; (13.2) *)
and instantiate_parser_obj_decl (ccenv : CCEnv.t) (sto : Sto.t) (ictx : ICtx.t)
    (path : Path.t) (decl : decl) =
  match decl.it with
  | InstD { id; typ; args; _ } ->
      instantiate_from_obj_decl ccenv sto ictx path id.it typ args
  | ConstD { id; typ; value; _ } ->
      let ictx = load_obj_const ictx id.it typ value in
      (sto, ictx)
  | VarD { id; typ; _ } ->
      let ictx = load_obj_var ictx id.it typ in
      (sto, ictx)
  (* There is no syntax for specifying parameters that are value-sets (Appendix F) *)
  | ValueSetD { id; _ } ->
      let path = path @ [ id.it ] in
      (* (TODO) What should be the runtime representation of value set? *)
      let obj = Object.ValueSetO in
      let value = Value.RefV path in
      let ictx = ICtx.add_var_obj id.it value ictx in
      let sto = Sto.add path obj sto in
      (sto, ictx)
  | _ -> failwith "(instantiate_parser_obj_decl) Unexpected declaration."

(* controlLocalDeclaration
   : constantDeclaration | actionDeclaration
   | tableDeclaration | instantiation
   | variableDeclaration; (14) *)
and instantiate_control_obj_decl (ccenv : CCEnv.t) (sto : Sto.t) (ictx : ICtx.t)
    (path : Path.t) (decl : decl) =
  match decl.it with
  | InstD { id; typ; args; _ } ->
      instantiate_from_obj_decl ccenv sto ictx path id.it typ args
  | ConstD { id; typ; value; _ } ->
      let ictx = load_obj_const ictx id.it typ value in
      (sto, ictx)
  | VarD { id; typ; _ } ->
      let ictx = load_obj_var ictx id.it typ in
      (sto, ictx)
  | ActionD { id; params; body; _ } ->
      let func = Func.ActionF { vis = env_to_vis ictx.env_obj; params; body } in
      let fid = FId.to_fid id params in
      let ictx = ICtx.add_func_obj fid func ictx in
      (sto, ictx)
  (* Each table evaluates to a table instance (18.2) *)
  (* There is no syntax for specifying parameters that are tables
     Tables are only intended to be used from within the control
     where they are defined (Appendix F) *)
  | TableD { id; table; _ } ->
      let path = path @ [ id.it ] in
      (* Build a dummy "apply" method for table *)
      let apply = Func.TableF { vis_obj = ictx.vis_obj } in
      let obj = Object.TableO { table; mthd = apply } in
      let value = Value.RefV path in
      let ictx = ICtx.add_var_obj id.it value ictx in
      let sto = Sto.add path obj sto in
      (sto, ictx)
  | _ -> failwith "(instantiate_control_obj_decl) Unexpected declaration."

and instantiate_extern_obj_decl (ictx : ICtx.t) (decl : decl) =
  match decl.it with
  | MethodD { id; tparams; params; _ } ->
      let func =
        Func.ExternMethodF
          { vis_obj = env_to_vis ictx.env_obj; tparams; params }
      in
      let fid = FId.to_fid id params in
      ICtx.add_func_obj fid func ictx
  | AbstractD _ ->
      Format.eprintf "(TODO: instantiate_extern_obj_decl) Load extern object %a"
        (Syntax.Pp.pp_decl ~level:0)
        decl;
      ictx
  | _ -> assert false

let instantiate_glob_decl (ccenv : CCEnv.t) (sto : Sto.t) (ictx : ICtx.t)
    (decl : decl) =
  match decl.it with
  (* Explicit instantiation of a package *)
  | InstD { id; typ; args; _ } ->
      let sto, ictx =
        instantiate_from_glob_decl ccenv sto ictx id.it typ args
      in
      (ccenv, sto, ictx)
  (* Load declaration to environments *)
  | _ ->
      let ccenv, ictx = load_glob_decl ccenv ictx decl in
      (ccenv, sto, ictx)

let instantiate_program (program : program) =
  let ccenv = CCEnv.empty in
  let sto = Sto.empty in
  let ictx = ICtx.empty in
  let ccenv, sto, ictx =
    List.fold_left
      (fun (ccenv, sto, ictx) decl -> instantiate_glob_decl ccenv sto ictx decl)
      (ccenv, sto, ictx) program
  in
  let ctx =
    Ctx.init ([], ("", [])) ictx.env_glob ictx.env_obj (TDEnv.empty, [])
  in
  (ccenv, sto, ctx)
