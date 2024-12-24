module L = Lang.Ast
module Value = Runtime.Value
module Types = Runtime.Tdomain.Types
module Type = Types.Type
module Ctk = Runtime.Domain.Ctk
module Envs = Runtime.Envs
module F = Format
open Util.Source
open Util.Error

let check = check_checker
let error_no_info = error_checker_no_info

(* Global counter for unique identifiers *)

let tick = ref 0
let refresh () = tick := 0

let fresh () =
  let id = !tick in
  tick := !tick + 1;
  id

(* Context is consisted of layers of environments *)

type cursor = Global | Block | Local

(* Defining each layer *)

type blockkind = Empty | Extern | Parser | Control | Package

type localkind =
  | Empty
  | ExternFunction
  | Function of Types.Type.t
  | Action
  | ExternMethod
  | ExternAbstractMethod of Types.Type.t
  | ParserState
  | ControlApplyMethod
  | TableApplyMethod

type gt = {
  cdenv : Envs.CDEnv.t;
  tdenv : Envs.TDEnv.t;
  fdenv : Envs.FDEnv.t;
  frame : Envs.VEnv.t * Envs.TEnv.t;
}

type bt = {
  kind : blockkind;
  tdenv : Envs.TDEnv.t;
  fdenv : Envs.FDEnv.t;
  frame : Envs.VEnv.t * Envs.TEnv.t;
}

type lt = {
  kind : localkind;
  tdenv : Envs.TDEnv.t;
  frames : (Envs.VEnv.t * Envs.TEnv.t) list;
}

type t = { global : gt; block : bt; local : lt }

let empty_gt =
  {
    cdenv = Envs.CDEnv.empty;
    tdenv = Envs.TDEnv.empty;
    fdenv = Envs.FDEnv.empty;
    frame = (Envs.VEnv.empty, Envs.TEnv.empty);
  }

let empty_bt =
  {
    kind = Empty;
    tdenv = Envs.TDEnv.empty;
    fdenv = Envs.FDEnv.empty;
    frame = (Envs.VEnv.empty, Envs.TEnv.empty);
  }

let empty_lt = { kind = Empty; tdenv = Envs.TDEnv.empty; frames = [] }
let empty = { global = empty_gt; block = empty_bt; local = empty_lt }

(* Frame management *)

let enter_frame ctx =
  {
    ctx with
    local =
      {
        ctx.local with
        frames = (Envs.VEnv.empty, Envs.TEnv.empty) :: ctx.local.frames;
      };
  }

let exit_frame ctx =
  match ctx.local.frames with
  | [] -> "(exit_frame) no frame to exit from" |> error_no_info
  | _ :: frames -> { ctx with local = { ctx.local with frames } }

(* Setters *)

let set_blockkind kind ctx = { ctx with block = { ctx.block with kind } }
let set_localkind kind ctx = { ctx with local = { ctx.local with kind } }

(* Getters *)

let rec get_tparams cursor ctx =
  match cursor with
  | Global -> []
  | Block -> Envs.TDEnv.bindings ctx.block.tdenv |> List.map fst
  | Local ->
      (Envs.TDEnv.bindings ctx.local.tdenv |> List.map fst)
      @ get_tparams Block ctx

(* Adders *)

let add_consdef cid cd ctx =
  let cdenv = Envs.CDEnv.add_nodup_overloaded cid cd ctx.global.cdenv in
  { ctx with global = { ctx.global with cdenv } }

let add_tparam cursor tparam ctx =
  match cursor with
  | Global ->
      "(add_tparam) global cursor cannot be type-parameterized" |> error_no_info
  | Block ->
      {
        ctx with
        block =
          {
            ctx.block with
            tdenv =
              Envs.TDEnv.add tparam.it (Types.MonoD (VarT tparam.it))
                ctx.block.tdenv;
          };
      }
  | Local ->
      {
        ctx with
        local =
          {
            ctx.local with
            tdenv =
              Envs.TDEnv.add tparam.it (Types.MonoD (VarT tparam.it))
                ctx.local.tdenv;
          };
      }

let add_tparams cursor tparams ctx =
  List.fold_left (fun ctx tparam -> add_tparam cursor tparam ctx) ctx tparams

let add_typedef cursor tid td ctx =
  match cursor with
  | Global ->
      let tdenv = Envs.TDEnv.add_nodup tid td ctx.global.tdenv in
      { ctx with global = { ctx.global with tdenv } }
  | Block ->
      let tdenv = Envs.TDEnv.add_nodup tid td ctx.block.tdenv in
      { ctx with block = { ctx.block with tdenv } }
  | Local ->
      let tdenv = Envs.TDEnv.add_nodup tid td ctx.local.tdenv in
      { ctx with local = { ctx.local with tdenv } }

let add_funcdef_non_overload cursor fid fd ctx =
  match cursor with
  | Global ->
      let fdenv = Envs.FDEnv.add_nodup_non_overloaded fid fd ctx.global.fdenv in
      { ctx with global = { ctx.global with fdenv } }
  | Block ->
      let fdenv = Envs.FDEnv.add_nodup_non_overloaded fid fd ctx.block.fdenv in
      { ctx with block = { ctx.block with fdenv } }
  | Local ->
      "(add_funcdef_non_overload) local cursor cannot have function definitions"
      |> error_no_info

let add_funcdef_overload cursor fid fd ctx =
  match cursor with
  | Global ->
      let fdenv = Envs.FDEnv.add_nodup_overloaded fid fd ctx.global.fdenv in
      { ctx with global = { ctx.global with fdenv } }
  | Block ->
      let fdenv = Envs.FDEnv.add_nodup_overloaded fid fd ctx.block.fdenv in
      { ctx with block = { ctx.block with fdenv } }
  | Local ->
      "(add_funcdef_overload) Local cursor cannot have function definitions"
      |> error_no_info

let add_value cursor id value ctx =
  match cursor with
  | Global ->
      let venv, tenv = ctx.global.frame in
      let venv = Envs.VEnv.add_nodup id value venv in
      { ctx with global = { ctx.global with frame = (venv, tenv) } }
  | Block ->
      let venv, tenv = ctx.block.frame in
      let venv = Envs.VEnv.add_nodup id value venv in
      { ctx with block = { ctx.block with frame = (venv, tenv) } }
  | Local ->
      let frames = ctx.local.frames in
      let (venv, tenv), frames =
        if frames = [] then ((Envs.VEnv.empty, Envs.TEnv.empty), [])
        else (List.hd frames, List.tl frames)
      in
      let venv = Envs.VEnv.add_nodup id value venv in
      let frame = (venv, tenv) in
      { ctx with local = { ctx.local with frames = frame :: frames } }

let add_rtype cursor id typ dir ctk ctx =
  check
    (implies (id = "main")
       (cursor = Global
       && match Type.canon typ with Types.PackageT _ -> true | _ -> false))
    "(add_rtype) main is reserved for a package instance at the top level";
  match cursor with
  | Global ->
      let venv, tenv = ctx.global.frame in
      let tenv = Envs.TEnv.add_nodup id (typ, dir, ctk) tenv in
      { ctx with global = { ctx.global with frame = (venv, tenv) } }
  | Block ->
      let venv, tenv = ctx.block.frame in
      let tenv = Envs.TEnv.add_nodup id (typ, dir, ctk) tenv in
      { ctx with block = { ctx.block with frame = (venv, tenv) } }
  | Local ->
      let frames = ctx.local.frames in
      let (venv, tenv), frames =
        if frames = [] then ((Envs.VEnv.empty, Envs.TEnv.empty), [])
        else (List.hd frames, List.tl frames)
      in
      let tenv = Envs.TEnv.add_nodup id (typ, dir, ctk) tenv in
      let frame = (venv, tenv) in
      { ctx with local = { ctx.local with frames = frame :: frames } }

let add_param cursor param ctx =
  let id, dir, typ, _, _ = param.it in
  let ctk = if dir.it = Lang.Ast.No then Ctk.CTK else Ctk.DYN in
  add_rtype cursor id.it typ.it dir.it ctk ctx

let add_params cursor params ctx =
  List.fold_left (fun ctx param -> add_param cursor param ctx) ctx params

let add_cparams = add_params

(* Removers *)

let remove_rtype cursor id ctx =
  match cursor with
  | Global ->
      let venv, tenv = ctx.global.frame in
      {
        ctx with
        global = { ctx.global with frame = (venv, Envs.TEnv.remove id tenv) };
      }
  | Block ->
      let venv, tenv = ctx.block.frame in
      {
        ctx with
        block = { ctx.block with frame = (venv, Envs.TEnv.remove id tenv) };
      }
  | Local ->
      let frames = ctx.local.frames in
      let (venv, tenv), frames =
        if frames = [] then ((Envs.VEnv.empty, Envs.TEnv.empty), [])
        else (List.hd frames, List.tl frames)
      in
      let frame = (venv, Envs.TEnv.remove id tenv) in
      { ctx with local = { ctx.local with frames = frame :: frames } }

(* Finders *)

let find_cont finder cursor id ctx = function
  | Some value -> Some value
  | None -> finder cursor id ctx

(* Finder for type definition *)

let rec find_typedef_opt cursor tid ctx =
  match cursor with
  | Global -> Envs.TDEnv.find_opt tid ctx.global.tdenv
  | Block ->
      Envs.TDEnv.find_opt tid ctx.block.tdenv
      |> find_cont find_typedef_opt Global tid ctx
  | Local ->
      Envs.TDEnv.find_opt tid ctx.local.tdenv
      |> find_cont find_typedef_opt Block tid ctx

let find_typedef cursor tid ctx = find_typedef_opt cursor tid ctx |> Option.get

(* Finder for function definition *)

let rec find_funcdef_opt cursor (fname, args) ctx =
  match cursor with
  | Global -> Envs.FDEnv.find_opt (fname, args) ctx.global.fdenv
  | Block ->
      Envs.FDEnv.find_opt (fname, args) ctx.block.fdenv
      |> find_cont find_funcdef_opt Global (fname, args) ctx
  | Local -> find_funcdef_opt Block (fname, args) ctx

let find_funcdef cursor (fname, args) ctx =
  find_funcdef_opt cursor (fname, args) ctx |> Option.get

let rec find_funcdef_overloaded_opt cursor (fname, args) ctx =
  match cursor with
  | Global -> Envs.FDEnv.find_overloaded_opt (fname, args) ctx.global.fdenv
  | Block ->
      Envs.FDEnv.find_overloaded_opt (fname, args) ctx.block.fdenv
      |> find_cont find_funcdef_overloaded_opt Global (fname, args) ctx
  | Local -> find_funcdef_overloaded_opt Block (fname, args) ctx

let find_funcdef_overloaded cursor (fname, args) ctx =
  find_funcdef_overloaded_opt cursor (fname, args) ctx |> Option.get

let rec find_funcdef_non_overloaded_opt cursor (fname, args) ctx =
  match cursor with
  | Global -> Envs.FDEnv.find_non_overloaded_opt (fname, args) ctx.global.fdenv
  | Block ->
      Envs.FDEnv.find_non_overloaded_opt (fname, args) ctx.block.fdenv
      |> find_cont find_funcdef_non_overloaded_opt Global (fname, args) ctx
  | Local -> find_funcdef_non_overloaded_opt Block (fname, args) ctx

let find_funcdef_non_overloaded cursor (fname, args) ctx =
  find_funcdef_non_overloaded_opt cursor (fname, args) ctx |> Option.get

(* Finder for constructor definition *)

let find_consdef_opt _cursor (cname, args) ctx =
  Envs.CDEnv.find_overloaded_opt (cname, args) ctx.global.cdenv

let find_consdef cursor (cname, args) ctx =
  find_consdef_opt cursor (cname, args) ctx |> Option.get

(* Finder for value *)

let rec find_value_opt cursor id ctx =
  match cursor with
  | Global ->
      let venv, _ = ctx.global.frame in
      Envs.VEnv.find_opt id venv
  | Block ->
      let venv, _ = ctx.block.frame in
      Envs.VEnv.find_opt id venv |> find_cont find_value_opt Global id ctx
  | Local ->
      let venvs = ctx.local.frames |> List.map fst in
      List.fold_left
        (fun value venv ->
          match value with
          | Some value -> Some value
          | None -> Envs.VEnv.find_opt id venv)
        None venvs
      |> find_cont find_value_opt Block id ctx

let find_value cursor id ctx = find_value_opt cursor id ctx |> Option.get

(* Finder for runtime type *)

let rec find_rtype_opt cursor id ctx =
  match cursor with
  | Global ->
      let _, tenv = ctx.global.frame in
      Envs.TEnv.find_opt id tenv
  | Block ->
      let _, tenv = ctx.block.frame in
      Envs.TEnv.find_opt id tenv |> find_cont find_rtype_opt Global id ctx
  | Local ->
      let tenvs = ctx.local.frames |> List.map snd in
      List.fold_left
        (fun typ tenv ->
          match typ with
          | Some typ -> Some typ
          | None -> Envs.TEnv.find_opt id tenv)
        None tenvs
      |> find_cont find_rtype_opt Block id ctx

let find_rtype cursor id ctx = find_rtype_opt cursor id ctx |> Option.get

(* Finder combinator *)

let find_opt finder_opt cursor var ctx =
  match var.it with
  | L.Top id -> finder_opt Global id.it ctx
  | L.Current id -> finder_opt cursor id.it ctx

let find finder_opt cursor var ctx =
  find_opt finder_opt cursor var ctx |> Option.get

let find_non_overloaded_opt finder_non_overloaded_opt cursor var args ctx =
  match var.it with
  | L.Top id -> finder_non_overloaded_opt Global (id.it, args) ctx
  | L.Current id -> finder_non_overloaded_opt cursor (id.it, args) ctx

let find_non_overloaded finder_non_overloaded_opt cursor var args ctx =
  find_non_overloaded_opt finder_non_overloaded_opt cursor var args ctx
  |> Option.get

let find_overloaded_opt finder_overloaded_opt cursor var args ctx =
  match var.it with
  | L.Top id -> finder_overloaded_opt Global (id.it, args) ctx
  | L.Current id -> finder_overloaded_opt cursor (id.it, args) ctx

let find_overloaded finder_overloaded_opt cursor var args ctx =
  find_overloaded_opt finder_overloaded_opt cursor var args ctx |> Option.get

(* Pretty-printer *)

let pp_frame fmt frame =
  let venv, tenv = frame in
  F.fprintf fmt "@[@[<v 0>[Values]:@ %a@]@\n@[<v 0>[Types]:@ %a@]@]"
    Envs.VEnv.pp venv Envs.TEnv.pp tenv

let pp_gt fmt (gt : gt) =
  F.fprintf fmt
    "@[@[<v 0>[[Global]]@]@\n\
     @[@[<v 0>[Constructors]:@ %a@]@\n\
     @[<v 0>[Typedefs]:@ %a@]@\n\
     @[<v 0>[Functions]:@ %a@]@\n\
     @[<v 0>[Frame]:@ %a@]@]" Envs.CDEnv.pp gt.cdenv Envs.TDEnv.pp gt.tdenv
    Envs.FDEnv.pp gt.fdenv pp_frame gt.frame

let pp_blockkind fmt (kind : blockkind) =
  match kind with
  | Empty -> F.fprintf fmt "Empty"
  | Extern -> F.fprintf fmt "Extern"
  | Parser -> F.fprintf fmt "Parser"
  | Control -> F.fprintf fmt "Control"
  | Package -> F.fprintf fmt "Package"

let pp_bt fmt (bt : bt) =
  F.fprintf fmt
    "@[@[<v 0>[[Block]]:@ %a@]@\n\
     @[<v 0>[Typedefs]:@ %a@]@\n\
     @[<v 0>[Functions]:@ %a@]@\n\
     @[<v 0>[Frame]:@ %a@]@]" pp_blockkind bt.kind Envs.TDEnv.pp bt.tdenv
    Envs.FDEnv.pp bt.fdenv pp_frame bt.frame

let pp_localkind fmt (kind : localkind) =
  match kind with
  | Empty -> F.fprintf fmt "Empty"
  | ExternFunction -> F.fprintf fmt "ExternFunction"
  | Function _ -> F.fprintf fmt "Function"
  | Action -> F.fprintf fmt "Action"
  | ExternMethod -> F.fprintf fmt "ExternMethod"
  | ExternAbstractMethod _ -> F.fprintf fmt "ExternAbstractMethod"
  | ParserState -> F.fprintf fmt "ParserState"
  | ControlApplyMethod -> F.fprintf fmt "ControlApplyMethod"
  | TableApplyMethod -> F.fprintf fmt "TableApplyMethod"

let pp_lt fmt (lt : lt) =
  F.fprintf fmt
    "@[@[<v 0>[[Local]]:@ %a@]@\n\
     @[<v 0>[Typedefs]:@ %a@]@\n\
     @[<v 0>[Frames]:@ %a@]@]" pp_localkind lt.kind Envs.TDEnv.pp lt.tdenv
    (F.pp_print_list pp_frame) lt.frames

let pp fmt ctx =
  F.fprintf fmt
    "@[@[<v 0>[[Context]]@]@\n\
     @[<v 0>[Global]:@ %a@]@\n\
     @[<v 0>[Block]:@ %a@]@\n\
     @[<v 0>[Local]:@ %a@]@]" pp_gt ctx.global pp_bt ctx.block pp_lt ctx.local
