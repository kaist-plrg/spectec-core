open Syntax.Ast
open Util.Source
open Types
module Value = Runtime.Value

(* Context is consisted of layers of environments *)

type cursor = Global | Block | Local

(* Defining each layer *)

type blockkind = Empty | Extern | Parser | Control | Package

type localkind =
  | Empty
  | ExternFunction
  | Function of Type.t
  | Action
  | ExternMethod
  | ExternAbstractMethod of Type.t
  | ParserState
  | ControlApplyMethod
  | TableMethod

type gt = {
  cdenv : CDEnv.t;
  tdenv : TDEnv.t;
  fdenv : FDEnv.t;
  frame : VEnv.t * TEnv.t;
}

type bt = {
  id : id';
  tparams : tparam' list;
  kind : blockkind;
  tdenv : TDEnv.t;
  fdenv : FDEnv.t;
  frame : VEnv.t * TEnv.t;
}

type lt = {
  id : id';
  tparams : tparam' list;
  kind : localkind;
  tdenv : TDEnv.t;
  frames : (VEnv.t * TEnv.t) list;
}

type t = { global : gt; block : bt; local : lt }

let empty_gt =
  {
    cdenv = CDEnv.empty;
    tdenv = TDEnv.empty;
    fdenv = FDEnv.empty;
    frame = (VEnv.empty, TEnv.empty);
  }

let empty_bt =
  {
    id = "";
    tparams = [];
    kind = Empty;
    tdenv = TDEnv.empty;
    fdenv = FDEnv.empty;
    frame = (VEnv.empty, TEnv.empty);
  }

let empty_lt =
  { id = ""; tparams = []; kind = Empty; tdenv = TDEnv.empty; frames = [] }

let empty = { global = empty_gt; block = empty_bt; local = empty_lt }

(* Frame management *)

let enter_frame ctx =
  {
    ctx with
    local =
      { ctx.local with frames = (VEnv.empty, TEnv.empty) :: ctx.local.frames };
  }

let exit_frame ctx =
  match ctx.local.frames with
  | [] ->
      Format.eprintf "(exit_frame) No frame to exit\n";
      assert false
  | _ :: frames -> { ctx with local = { ctx.local with frames } }

(* Setters *)

let set_id cursor id ctx =
  match cursor with
  | Global ->
      Format.eprintf "(set_id) Global cursor has no identifier\n";
      assert false
  | Block -> { ctx with block = { ctx.block with id } }
  | Local -> { ctx with local = { ctx.local with id } }

let set_blockkind kind ctx = { ctx with block = { ctx.block with kind } }
let set_localkind kind ctx = { ctx with local = { ctx.local with kind } }

(* Getters *)

let rec get_tparams cursor ctx =
  match cursor with
  | Global -> []
  | Block -> ctx.block.tparams
  | Local -> ctx.local.tparams @ get_tparams Block ctx

(* Adders *)

let add_consdef cid cd ctx =
  {
    ctx with
    global = { ctx.global with cdenv = CDEnv.add cid cd ctx.global.cdenv };
  }

let add_tparam cursor tparam ctx =
  match cursor with
  | Global ->
      Format.eprintf "(add_tparam) Global cursor cannot be type-parameterized\n";
      assert false
  | Block ->
      {
        ctx with
        block = { ctx.block with tparams = ctx.block.tparams @ [ tparam ] };
      }
  | Local ->
      {
        ctx with
        local = { ctx.local with tparams = ctx.local.tparams @ [ tparam ] };
      }

let add_tparams cursor tparams ctx =
  List.fold_left (fun ctx tparam -> add_tparam cursor tparam ctx) ctx tparams

let add_typedef cursor tid td ctx =
  match cursor with
  | Global ->
      {
        ctx with
        global = { ctx.global with tdenv = TDEnv.add tid td ctx.global.tdenv };
      }
  | Block ->
      {
        ctx with
        block = { ctx.block with tdenv = TDEnv.add tid td ctx.block.tdenv };
      }
  | Local ->
      {
        ctx with
        local = { ctx.local with tdenv = TDEnv.add tid td ctx.local.tdenv };
      }

let add_funcdef cursor fid fd ctx =
  match cursor with
  | Global ->
      {
        ctx with
        global = { ctx.global with fdenv = FDEnv.add fid fd ctx.global.fdenv };
      }
  | Block ->
      {
        ctx with
        block = { ctx.block with fdenv = FDEnv.add fid fd ctx.block.fdenv };
      }
  | Local ->
      Format.eprintf
        "(add_funcdef) Local cursor cannot have function definitions\n";
      assert false

let add_value cursor id value ctx =
  match cursor with
  | Global ->
      let venv, tenv = ctx.global.frame in
      {
        ctx with
        global = { ctx.global with frame = (VEnv.add id value venv, tenv) };
      }
  | Block ->
      let venv, tenv = ctx.block.frame in
      {
        ctx with
        block = { ctx.block with frame = (VEnv.add id value venv, tenv) };
      }
  | Local ->
      let frames = ctx.local.frames in
      let (venv, tenv), frames =
        if frames = [] then ((VEnv.empty, TEnv.empty), [])
        else (List.hd frames, List.tl frames)
      in
      let frame = (VEnv.add id value venv, tenv) in
      { ctx with local = { ctx.local with frames = frame :: frames } }

let add_type cursor id typ ctx =
  match cursor with
  | Global ->
      let venv, tenv = ctx.global.frame in
      {
        ctx with
        global = { ctx.global with frame = (venv, TEnv.add id typ tenv) };
      }
  | Block ->
      let venv, tenv = ctx.block.frame in
      {
        ctx with
        block = { ctx.block with frame = (venv, TEnv.add id typ tenv) };
      }
  | Local ->
      let frames = ctx.local.frames in
      let (venv, tenv), frames =
        if frames = [] then ((VEnv.empty, TEnv.empty), [])
        else (List.hd frames, List.tl frames)
      in
      let frame = (venv, TEnv.add id typ tenv) in
      { ctx with local = { ctx.local with frames = frame :: frames } }

(* Finders *)

let find_cont finder cursor id ctx = function
  | Some value -> Some value
  | None -> finder cursor id ctx

let rec find_tparam_opt cursor tparam ctx =
  match cursor with
  | Global -> None
  | Block -> List.find_opt (fun tp -> tp = tparam) ctx.block.tparams
  | Local ->
      List.find_opt (fun tp -> tp = tparam) ctx.local.tparams
      |> find_cont find_tparam_opt Block tparam ctx

let find_tparam cursor tparam ctx =
  find_tparam_opt cursor tparam ctx |> Option.get

let rec find_typedef_opt cursor tid ctx =
  match cursor with
  | Global -> TDEnv.find_opt tid ctx.global.tdenv
  | Block ->
      TDEnv.find_opt tid ctx.block.tdenv
      |> find_cont find_typedef_opt Global tid ctx
  | Local ->
      TDEnv.find_opt tid ctx.local.tdenv
      |> find_cont find_typedef_opt Block tid ctx

let find_typedef cursor tid ctx = find_typedef_opt cursor tid ctx |> Option.get

let rec find_funcdef_opt cursor (fid, args) ctx =
  match cursor with
  | Global -> FDEnv.find_opt (fid, args) ctx.global.fdenv
  | Block ->
      FDEnv.find_opt (fid, args) ctx.block.fdenv
      |> find_cont find_funcdef_opt Global (fid, args) ctx
  | Local -> find_funcdef_opt Block (fid, args) ctx

let find_funcdef cursor (fid, args) ctx =
  find_funcdef_opt cursor (fid, args) ctx |> Option.get

let rec find_value_opt cursor id ctx =
  match cursor with
  | Global ->
      let venv, _ = ctx.global.frame in
      VEnv.find_opt id venv
  | Block ->
      let venv, _ = ctx.block.frame in
      VEnv.find_opt id venv |> find_cont find_value_opt Global id ctx
  | Local ->
      let venvs = ctx.local.frames |> List.map fst in
      List.fold_left
        (fun value venv ->
          match value with
          | Some value -> Some value
          | None -> VEnv.find_opt id venv)
        None venvs
      |> find_cont find_value_opt Block id ctx

let find_value cursor id ctx = find_value_opt cursor id ctx |> Option.get

let rec find_type_opt cursor id ctx =
  match cursor with
  | Global ->
      let _, tenv = ctx.global.frame in
      TEnv.find_opt id tenv
  | Block ->
      let _, tenv = ctx.block.frame in
      TEnv.find_opt id tenv |> find_cont find_type_opt Global id ctx
  | Local ->
      let tenvs = ctx.local.frames |> List.map snd in
      List.fold_left
        (fun typ tenv ->
          match typ with Some typ -> Some typ | None -> TEnv.find_opt id tenv)
        None tenvs
      |> find_cont find_type_opt Block id ctx

let find_type cursor id ctx = find_type_opt cursor id ctx |> Option.get

let find_opt finder_opt cursor var ctx =
  match var.it with
  | Top id -> finder_opt Global id.it ctx
  | Current id -> finder_opt cursor id.it ctx

let find finder_opt cursor var ctx =
  find_opt finder_opt cursor var ctx |> Option.get

let find_overloaded_opt finder_overloaded_opt cursor var args ctx =
  match var.it with
  | Top id -> finder_overloaded_opt Global (id.it, args) ctx
  | Current id -> finder_overloaded_opt cursor (id.it, args) ctx

let find_overloaded finder_overloaded_opt cursor var args ctx =
  find_overloaded_opt finder_overloaded_opt cursor var args ctx |> Option.get

(* Pretty-printer *)

let pp_frame fmt frame =
  let venv, tenv = frame in
  Format.fprintf fmt "@[@[<v 0>[Values]:@ %a@]@\n@[<v 0>[Types]:@ %a@]@]"
    VEnv.pp venv TEnv.pp tenv

let pp_gt fmt (gt : gt) =
  Format.fprintf fmt
    "@[@[<v 0>[[Global]]@]@\n\
     @[@[<v 0>[Constructors]:@ %a@]@\n\
     @[<v 0>[Typedefs]:@ %a@]@\n\
     @[<v 0>[Functions]:@ %a@]@\n\
     @[<v 0>[Frame]:@ %a@]@]" CDEnv.pp gt.cdenv TDEnv.pp gt.tdenv FDEnv.pp
    gt.fdenv pp_frame gt.frame

let pp_bt fmt (bt : bt) =
  Format.fprintf fmt
    "@[@[<v 0>[Block %s<%s>]:@ %a@]@\n\
     @[<v 0>[Typedefs]:@ %a@]@\n\
     @[<v 0>[Functions]:@ %a@]@\n\
     @[<v 0>[Frame]:@ %a@]@]" bt.id
    (String.concat ", " bt.tparams)
    (fun fmt (kind : blockkind) ->
      match kind with
      | Empty -> Format.fprintf fmt "Empty"
      | Extern -> Format.fprintf fmt "Extern"
      | Parser -> Format.fprintf fmt "Parser"
      | Control -> Format.fprintf fmt "Control"
      | Package -> Format.fprintf fmt "Package")
    bt.kind TDEnv.pp bt.tdenv FDEnv.pp bt.fdenv pp_frame bt.frame

let pp_lt fmt (lt : lt) =
  Format.fprintf fmt
    "@[@[<v 0>[Local %s<%s>]:@ %a@]@\n\
     @[<v 0>[Typedefs]:@ %a@]@\n\
     @[<v 0>[Frames]:@ %a@]@]" lt.id
    (String.concat ", " lt.tparams)
    (fun fmt kind ->
      match (kind : localkind) with
      | Empty -> Format.fprintf fmt "Empty"
      | ExternFunction -> Format.fprintf fmt "ExternFunction"
      | Function _ -> Format.fprintf fmt "Function"
      | Action -> Format.fprintf fmt "Action"
      | ExternMethod -> Format.fprintf fmt "ExternMethod"
      | ExternAbstractMethod _ -> Format.fprintf fmt "ExternAbstractMethod"
      | ParserState -> Format.fprintf fmt "ParserState"
      | ControlApplyMethod -> Format.fprintf fmt "ControlApplyMethod"
      | TableMethod -> Format.fprintf fmt "TableMethod")
    lt.kind TDEnv.pp lt.tdenv
    (Format.pp_print_list pp_frame)
    lt.frames

let pp fmt ctx =
  Format.fprintf fmt
    "@[@[<v 0>[[Context]]@]@\n\
     @[<v 0>[Global]:@ %a@]@\n\
     @[<v 0>[Block]:@ %a@]@\n\
     @[<v 0>[Local]:@ %a@]@]" pp_gt ctx.global pp_bt ctx.block pp_lt ctx.local
