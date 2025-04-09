module L = Lang.Ast
module Value = Runtime_value.Value
module Types = Runtime_type.Types
module Type = Types.Type
module Ctk = Il.Ctk
module Envs = Runtime_static.Envs
module Frame = Envs.Frame
module TDEnv = Envs.TDEnv
module FDEnv = Envs.FDEnv
module CDEnv = Envs.CDEnv
module F = Format
open Util.Pp
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

type gt = { cdenv : CDEnv.t; tdenv : TDEnv.t; fdenv : FDEnv.t; frame : Frame.t }

type bt = {
  kind : blockkind;
  tdenv : TDEnv.t;
  fdenv : FDEnv.t;
  frame : Frame.t;
}

type lt = { kind : localkind; tdenv : TDEnv.t; frames : Frame.t list }
type t = { global : gt; block : bt; local : lt }

(* Constructors *)

let empty_gt =
  {
    cdenv = CDEnv.empty;
    tdenv = TDEnv.empty;
    fdenv = FDEnv.empty;
    frame = Frame.empty;
  }

let empty_bt =
  {
    kind = Empty;
    tdenv = TDEnv.empty;
    fdenv = FDEnv.empty;
    frame = Frame.empty;
  }

let empty_lt = { kind = Empty; tdenv = TDEnv.empty; frames = [ Frame.empty ] }
let empty = { global = empty_gt; block = empty_bt; local = empty_lt }

(* Frame management *)

let enter_frame ctx =
  let frames = Frame.empty :: ctx.local.frames in
  { ctx with local = { ctx.local with frames } }

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
  | Block -> TDEnv.bindings ctx.block.tdenv |> List.map fst
  | Local ->
      (TDEnv.bindings ctx.local.tdenv |> List.map fst) @ get_tparams Block ctx

(* Adders *)

(* Adders for constructor definitions *)

let add_consdef cid cd ctx =
  let cdenv = CDEnv.add_nodup_overloaded cid cd ctx.global.cdenv in
  { ctx with global = { ctx.global with cdenv } }

(* Adders for type definitions *)

let add_tparam cursor tparam ctx =
  match cursor with
  | Global ->
      "(add_tparam) global layer cannot be type-parameterized" |> error_no_info
  | Block ->
      let tdenv =
        TDEnv.add_nodup tparam.it (Types.MonoD (VarT tparam.it)) ctx.block.tdenv
      in
      { ctx with block = { ctx.block with tdenv } }
  | Local ->
      let tdenv =
        TDEnv.add_nodup tparam.it (Types.MonoD (VarT tparam.it)) ctx.local.tdenv
      in
      { ctx with local = { ctx.local with tdenv } }

let add_tparams cursor tparams ctx =
  List.fold_left (fun ctx tparam -> add_tparam cursor tparam ctx) ctx tparams

let add_typdef cursor tid td ctx =
  match cursor with
  | Global ->
      let tdenv = TDEnv.add_nodup tid td ctx.global.tdenv in
      { ctx with global = { ctx.global with tdenv } }
  | Block ->
      let tdenv = TDEnv.add_nodup tid td ctx.block.tdenv in
      { ctx with block = { ctx.block with tdenv } }
  | Local ->
      let tdenv = TDEnv.add_nodup tid td ctx.local.tdenv in
      { ctx with local = { ctx.local with tdenv } }

(* Adders for function definitions *)

let add_funcdef_non_overload cursor fid fd ctx =
  match cursor with
  | Global ->
      let fdenv = FDEnv.add_nodup_non_overloaded fid fd ctx.global.fdenv in
      { ctx with global = { ctx.global with fdenv } }
  | Block ->
      let fdenv = FDEnv.add_nodup_non_overloaded fid fd ctx.block.fdenv in
      { ctx with block = { ctx.block with fdenv } }
  | Local ->
      "(add_funcdef_non_overload) local layer cannot have function definitions"
      |> error_no_info

let add_funcdef_overload cursor fid fd ctx =
  match cursor with
  | Global ->
      let fdenv = FDEnv.add_nodup_overloaded fid fd ctx.global.fdenv in
      { ctx with global = { ctx.global with fdenv } }
  | Block ->
      let fdenv = FDEnv.add_nodup_overloaded fid fd ctx.block.fdenv in
      { ctx with block = { ctx.block with fdenv } }
  | Local ->
      "(add_funcdef_overload) local layer cannot have function definitions"
      |> error_no_info

(* Adders for types and values *)

let add_stype cursor id typ dir ctk value ctx =
  check
    (implies (id = "main")
       (cursor = Global
       && match Type.canon typ with Types.PackageT _ -> true | _ -> false))
    "(add_stype) main is reserved for a package instance at the top level";
  match cursor with
  | Global ->
      let frame = Frame.add_nodup id (typ, dir, ctk, value) ctx.global.frame in
      { ctx with global = { ctx.global with frame } }
  | Block ->
      let frame = Frame.add_nodup id (typ, dir, ctk, value) ctx.block.frame in
      { ctx with block = { ctx.block with frame } }
  | Local ->
      let frames = ctx.local.frames in
      let frame, frames = (List.hd frames, List.tl frames) in
      let frame = Frame.add_nodup id (typ, dir, ctk, value) frame in
      let frames = frame :: frames in
      { ctx with local = { ctx.local with frames } }

let add_type cursor id typ dir ctk = add_stype cursor id typ dir ctk None

let add_param cursor param ctx =
  let id, dir, typ, _, _ = param.it in
  let ctk = if dir.it = Lang.Ast.No then Ctk.CTK else Ctk.DYN in
  add_stype cursor id.it typ.it dir.it ctk None ctx

let add_params cursor params ctx =
  List.fold_left (fun ctx param -> add_param cursor param ctx) ctx params

let add_cparams = add_params

(* Finders *)

let find_cont finder cursor id ctx = function
  | Some value -> Some value
  | None -> finder cursor id ctx

(* Finders for constructor definitions *)

let find_consdef_opt _cursor (cname, args) ctx =
  CDEnv.find_func_opt (cname, args) ctx.global.cdenv

let find_consdef cursor (cname, args) ctx =
  find_consdef_opt cursor (cname, args) ctx |> Option.get

(* Finders for type definitions *)

let rec find_typdef_opt cursor tid ctx =
  match cursor with
  | Global -> TDEnv.find_opt tid ctx.global.tdenv
  | Block ->
      TDEnv.find_opt tid ctx.block.tdenv
      |> find_cont find_typdef_opt Global tid ctx
  | Local ->
      TDEnv.find_opt tid ctx.local.tdenv
      |> find_cont find_typdef_opt Block tid ctx

let find_typdef cursor tid ctx = find_typdef_opt cursor tid ctx |> Option.get

(* Finders for function definitions *)

let rec find_funcdef_opt cursor (fname, args) ctx =
  match cursor with
  | Global -> FDEnv.find_func_opt (fname, args) ctx.global.fdenv
  | Block ->
      FDEnv.find_func_opt (fname, args) ctx.block.fdenv
      |> find_cont find_funcdef_opt Global (fname, args) ctx
  | Local -> find_funcdef_opt Block (fname, args) ctx

let find_funcdef cursor (fname, args) ctx =
  find_funcdef_opt cursor (fname, args) ctx |> Option.get

let rec find_funcdef_by_name_opt cursor (fname, args) ctx =
  match cursor with
  | Global -> FDEnv.find_func_by_name_opt fname ctx.global.fdenv
  | Block ->
      FDEnv.find_func_by_name_opt fname ctx.block.fdenv
      |> find_cont find_funcdef_by_name_opt Global (fname, args) ctx
  | Local -> find_funcdef_by_name_opt Block (fname, args) ctx

let find_funcdef_by_name cursor fname ctx =
  find_funcdef_by_name_opt cursor fname ctx |> Option.get

(* Finders for types and values *)

let rec find_stype_opt cursor id ctx =
  match cursor with
  | Global -> Frame.find_opt id ctx.global.frame
  | Block ->
      Frame.find_opt id ctx.block.frame
      |> find_cont find_stype_opt Global id ctx
  | Local ->
      List.fold_left
        (fun typ frame ->
          match typ with
          | Some typ -> Some typ
          | None -> Frame.find_opt id frame)
        None ctx.local.frames
      |> find_cont find_stype_opt Block id ctx

let find_stype cursor id ctx = find_stype_opt cursor id ctx |> Option.get

let rec find_value_opt cursor id ctx =
  match cursor with
  | Global ->
      Option.bind (Frame.find_opt id ctx.global.frame) (fun (_, _, _, value) ->
          value)
  | Block ->
      Option.bind (Frame.find_opt id ctx.block.frame) (fun (_, _, _, value) ->
          value)
      |> find_cont find_value_opt Global id ctx
  | Local ->
      List.fold_left
        (fun value frame ->
          match value with
          | Some value -> Some value
          | None ->
              Option.bind (Frame.find_opt id frame) (fun (_, _, _, value) ->
                  value))
        None ctx.local.frames
      |> find_cont find_value_opt Block id ctx

let find_value cursor id ctx = find_value_opt cursor id ctx |> Option.get

(* Finder combinators *)

let find_opt finder_opt cursor var ctx =
  match var.it with
  | L.Top id -> finder_opt Global id.it ctx
  | L.Current id -> finder_opt cursor id.it ctx

let find finder_opt cursor var ctx =
  find_opt finder_opt cursor var ctx |> Option.get

let find_f_opt finder_f_opt cursor var args ctx =
  match var.it with
  | L.Top id -> finder_f_opt Global (id.it, args) ctx
  | L.Current id -> finder_f_opt cursor (id.it, args) ctx

let find_f finder_f_opt cursor var args ctx =
  find_f_opt finder_f_opt cursor var args ctx |> Option.get

(* Pretty-printer *)

let pp_gt ?(level = 0) fmt (gt : gt) =
  F.fprintf fmt
    "%s[[Global Layer]]\n%s[Constructors]%a\n%s[Functions]%a\n%s[Frame]%a\n"
    (indent level)
    (indent (level + 1))
    (CDEnv.pp ~level:(level + 2))
    gt.cdenv
    (indent (level + 1))
    (FDEnv.pp ~level:(level + 2))
    gt.fdenv
    (indent (level + 1))
    (Frame.pp ~level:(level + 2))
    gt.frame

let pp_blockkind fmt (kind : blockkind) =
  match kind with
  | Empty -> F.fprintf fmt "Empty"
  | Extern -> F.fprintf fmt "Extern"
  | Parser -> F.fprintf fmt "Parser"
  | Control -> F.fprintf fmt "Control"
  | Package -> F.fprintf fmt "Package"

let pp_bt ?(level = 0) fmt (bt : bt) =
  F.fprintf fmt
    "%s[[Block Layer %a]]\n%s[Typedefs]%a\n%s[Functions]%a\n%s[Frame]%a\n"
    (indent level) pp_blockkind bt.kind
    (indent (level + 1))
    (TDEnv.pp ~level:(level + 2))
    bt.tdenv
    (indent (level + 1))
    (FDEnv.pp ~level:(level + 2))
    bt.fdenv
    (indent (level + 1))
    (Frame.pp ~level:(level + 2))
    bt.frame

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

let pp_lt ?(level = 0) fmt (lt : lt) =
  F.fprintf fmt "%s[[Local Layer %a]]\n%s[Typedefs]%a\n%s[Values]\n%a\n"
    (indent level) pp_localkind lt.kind
    (indent (level + 1))
    (TDEnv.pp ~level:(level + 2))
    lt.tdenv
    (indent (level + 1))
    (pp_list ~level:(level + 2) Frame.pp ~sep:Nl)
    lt.frames

let pp ?(level = 0) fmt ctx =
  F.fprintf fmt "===== Context =====\n%a%a%a"
    (pp_gt ~level:(level + 1))
    ctx.global
    (pp_bt ~level:(level + 1))
    ctx.block
    (pp_lt ~level:(level + 1))
    ctx.local
