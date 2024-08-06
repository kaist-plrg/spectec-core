open Syntax.Ast
open Util.Source
open Types
module Value = Runtime.Value

(* Context is consisted of layers of environments *)

type layer = Global | Block | Local
type frame = TDEnv.t * FDEnv.t * VEnv.t * TEnv.t

type t = {
  cons : CDEnv.t;
  global : frame;
  block : tparam' list * frame;
  local : tparam' list * frame list;
}

let empty_frame = (TDEnv.empty, FDEnv.empty, VEnv.empty, TEnv.empty)

let empty =
  {
    cons = CDEnv.empty;
    global = empty_frame;
    block = ([], empty_frame);
    local = ([], []);
  }

(* Getters *)

let rec get_tparams layer ctx =
  match layer with
  | Global -> []
  | Block ->
      let tparams, _ = ctx.block in
      tparams
  | Local ->
      let tparams, _ = ctx.local in
      tparams @ get_tparams Block ctx

(* Adders *)

let add_tparam layer tparam ctx =
  match layer with
  | Global ->
      Format.eprintf "(add_tparam) Global layer cannot be type-parameterized.";
      assert false
  | Block ->
      let tparams, frame = ctx.block in
      { ctx with block = (tparams @ [ tparam ], frame) }
  | Local ->
      let tparams, frames = ctx.local in
      { ctx with local = (tparams @ [ tparam ], frames) }

let add_typedef layer tid td ctx =
  match layer with
  | Global ->
      let tdenv, fdenv, venv, tenv = ctx.global in
      { ctx with global = (TDEnv.add tid td tdenv, fdenv, venv, tenv) }
  | Block ->
      let tparams, (tdenv, fdenv, venv, tenv) = ctx.block in
      {
        ctx with
        block = (tparams, (TDEnv.add tid td tdenv, fdenv, venv, tenv));
      }
  | Local ->
      let tparams, frames = ctx.local in
      let frame, frames = (List.hd frames, List.tl frames) in
      let tdenv, fdenv, venv, tenv = frame in
      let frame = (TDEnv.add tid td tdenv, fdenv, venv, tenv) in
      { ctx with local = (tparams, frame :: frames) }

let add_funcdef layer fid fd ctx =
  match layer with
  | Global ->
      let tdenv, fdenv, venv, tenv = ctx.global in
      { ctx with global = (tdenv, FDEnv.add fid fd fdenv, venv, tenv) }
  | Block ->
      let tparams, (tdenv, fdenv, venv, tenv) = ctx.block in
      {
        ctx with
        block = (tparams, (tdenv, FDEnv.add fid fd fdenv, venv, tenv));
      }
  | Local ->
      let tparams, frames = ctx.local in
      let frame, frames = (List.hd frames, List.tl frames) in
      let tdenv, fdenv, venv, tenv = frame in
      let frame = (tdenv, FDEnv.add fid fd fdenv, venv, tenv) in
      { ctx with local = (tparams, frame :: frames) }

let add_value layer id value ctx =
  match layer with
  | Global ->
      let tdenv, fdenv, venv, tenv = ctx.global in
      { ctx with global = (tdenv, fdenv, VEnv.add id value venv, tenv) }
  | Block ->
      let tparams, (tdenv, fdenv, venv, tenv) = ctx.block in
      {
        ctx with
        block = (tparams, (tdenv, fdenv, VEnv.add id value venv, tenv));
      }
  | Local ->
      let tparams, frames = ctx.local in
      let frame, frames = (List.hd frames, List.tl frames) in
      let tdenv, fdenv, venv, tenv = frame in
      let frame = (tdenv, fdenv, VEnv.add id value venv, tenv) in
      { ctx with local = (tparams, frame :: frames) }

let add_type layer id typ ctx =
  match layer with
  | Global ->
      let tdenv, fdenv, venv, tenv = ctx.global in
      { ctx with global = (tdenv, fdenv, venv, TEnv.add id typ tenv) }
  | Block ->
      let tparams, (tdenv, fdenv, venv, tenv) = ctx.block in
      { ctx with block = (tparams, (tdenv, fdenv, venv, TEnv.add id typ tenv)) }
  | Local ->
      let tparams, frames = ctx.local in
      let frame, frames = (List.hd frames, List.tl frames) in
      let tdenv, fdenv, venv, tenv = frame in
      let frame = (tdenv, fdenv, venv, TEnv.add id typ tenv) in
      { ctx with local = (tparams, frame :: frames) }

(* Finders *)

let find_cont finder layer id ctx = function
  | Some value -> Some value
  | None -> finder layer id ctx

let rec find_tparam_opt layer tparam ctx =
  match layer with
  | Global -> None
  | Block ->
      let tparams, _ = ctx.block in
      List.find_opt (fun tp -> tp = tparam) tparams
  | Local ->
      let tparams, _ = ctx.local in
      List.find_opt (fun tp -> tp = tparam) tparams
      |> find_cont find_tparam_opt Block tparam ctx

let find_tparam layer tparam ctx =
  find_tparam_opt layer tparam ctx |> Option.get

let rec find_typedef_opt layer tid ctx =
  match layer with
  | Global ->
      let tdenv, _, _, _ = ctx.global in
      TDEnv.find_opt tid tdenv
  | Block ->
      let _, (tdenv, _, _, _) = ctx.block in
      TDEnv.find_opt tid tdenv |> find_cont find_typedef_opt Global tid ctx
  | Local ->
      let _, frames = ctx.local in
      List.fold_left
        (fun td frame ->
          match td with
          | Some td -> Some td
          | None ->
              let tdenv, _, _, _ = frame in
              TDEnv.find_opt tid tdenv)
        None frames
      |> find_cont find_typedef_opt Block tid ctx

let find_typedef layer tid ctx = find_typedef_opt layer tid ctx |> Option.get

let rec find_value_opt layer id ctx =
  match layer with
  | Global ->
      let _, _, venv, _ = ctx.global in
      VEnv.find_opt id venv
  | Block ->
      let _, (_, _, venv, _) = ctx.block in
      VEnv.find_opt id venv |> find_cont find_value_opt Global id ctx
  | Local ->
      let _, frames = ctx.local in
      List.fold_left
        (fun value frame ->
          match value with
          | Some value -> Some value
          | None ->
              let _, _, venv, _ = frame in
              VEnv.find_opt id venv)
        None frames
      |> find_cont find_value_opt Block id ctx

let find_value layer id ctx = find_value_opt layer id ctx |> Option.get

let find finder var ctx =
  match var.it with
  | Top id -> finder Global id.it ctx
  | Bare id -> finder Local id.it ctx

let find_opt finder_opt var ctx =
  match var.it with
  | Top id -> finder_opt Global id.it ctx
  | Bare id -> finder_opt Local id.it ctx

(* Pretty-printer *)

let pp_frame fmt frame =
  let tdenv, fdenv, venv, tenv = frame in
  Format.fprintf fmt
    "@[@[<v 0>Typedefs:@ %a@]@\n\
     @[<v 0>Functions:@ %a@]@\n\
     @[<v 0>Values:@ %a@]@\n\
     @[<v 0>Types:@ %a@]@]" TDEnv.pp tdenv FDEnv.pp fdenv VEnv.pp venv TEnv.pp
    tenv

let pp fmt ctx =
  let tparams_block, frame_block = ctx.block in
  let tparams_local, frames_local = ctx.local in
  Format.fprintf fmt
    "@[@[<v 0>Constructors:@ %a@]@\n\
     @[<v 0>Global:@ %a@]@\n\
     @[<v 0>Block<%s>:@ %a@]@\n\
     @[<v 0>Local<%s>:@ %a@]@]" CDEnv.pp ctx.cons pp_frame ctx.global
    (String.concat ", " tparams_block)
    pp_frame frame_block
    (String.concat ", " tparams_local)
    (Format.pp_print_list pp_frame)
    frames_local
