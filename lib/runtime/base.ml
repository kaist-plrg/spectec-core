open Syntax.Ast
open Util.Source
open Domain

(* Visibility of type variables, variables, and function names *)

module TDVis = MakeVis (Id)
module Vis = MakeVis (Id)
module FVis = MakeVis (FId)

type vis = TDVis.t * Vis.t * FVis.t

let vis_empty = (TDVis.empty, Vis.empty, FVis.empty)

(* Runtime representation of values *)

module Value = struct
  type t =
    | BoolV of bool
    | AIntV of Bigint.t
    | IntV of Bigint.t * Bigint.t
    | BitV of Bigint.t * Bigint.t
    | VBitV of Bigint.t * Bigint.t * Bigint.t
    | StrV of string
    | ErrV of member'
    | MatchKindV of member'
    | StackV of (t list * Bigint.t * Bigint.t)
    | TupleV of t list
    | StructV of (member' * t) list
    | HeaderV of bool * (member' * t) list
    | UnionV of (member' * t) list
    | EnumFieldV of id' * member'
    | SEnumFieldV of id' * member' * t
    | RefV of path'

  let rec pp fmt = function
    | BoolV b -> Format.fprintf fmt "%b" b
    | AIntV i -> Format.fprintf fmt "%s" (Bigint.to_string i)
    | IntV (w, i) ->
        Format.fprintf fmt "%ss%s" (Bigint.to_string w) (Bigint.to_string i)
    | BitV (w, i) ->
        Format.fprintf fmt "%sw%s" (Bigint.to_string w) (Bigint.to_string i)
    | VBitV (_mw, w, i) ->
        Format.fprintf fmt "%sv%s" (Bigint.to_string w) (Bigint.to_string i)
    | StrV s -> Format.fprintf fmt "\"%s\"" s
    | ErrV s -> Format.fprintf fmt "%s" s
    | MatchKindV s -> Format.fprintf fmt "%s" s
    | StackV (vs, _i, s) ->
        Format.fprintf fmt "%a[%s]"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             pp)
          vs (Bigint.to_string s)
    | TupleV vs ->
        Format.fprintf fmt "(%a)"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             pp)
          vs
    | StructV fs ->
        Format.fprintf fmt "struct { @[<hv>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (m, v) -> Format.fprintf fmt "%s: %a" m pp v))
          fs
    | HeaderV (v, fs) ->
        Format.fprintf fmt "header { %s, @[<hv>%a@] }"
          (if v then "valid" else "invalid")
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (m, v) -> Format.fprintf fmt "%s: %a" m pp v))
          fs
    | UnionV fs ->
        Format.fprintf fmt "union { @[<hv>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (m, v) -> Format.fprintf fmt "%s: %a" m pp v))
          fs
    | EnumFieldV (_, m) -> Format.fprintf fmt "%s" m
    | SEnumFieldV (_, m, v) -> Format.fprintf fmt "%s(%a)" m pp v
    | RefV p -> Format.fprintf fmt "ref %s" (String.concat "." p)

  (* Getters *)

  let get_bool t : bool =
    match t with
    | BoolV value -> value
    | _ -> Format.asprintf "Not a bool value: %a" pp t |> failwith

  let get_num t : Bigint.t =
    match t with
    | AIntV value -> value
    | IntV (_, value) -> value
    | BitV (_, value) -> value
    | _ -> Format.asprintf "Not a int/bit value: %a" pp t |> failwith

  let rec get_width t =
    match t with
    | BoolV _ -> Bigint.one
    | IntV (width, _) | BitV (width, _) | VBitV (_, width, _) -> width
    | TupleV values ->
        List.fold_left
          (fun acc value -> Bigint.(acc + get_width value))
          Bigint.zero values
    | StructV fields | HeaderV (_, fields) ->
        let values = List.map snd fields in
        List.fold_left
          (fun acc value -> Bigint.(acc + get_width value))
          Bigint.zero values
    | _ -> Format.asprintf "Cannot get width of value: %a" pp t |> failwith

  let get_enum t =
    match t with
    | EnumFieldV (id, member) -> (id, member)
    | _ -> Format.asprintf "Not an enum value: %a" pp t |> failwith

  (* Aggregate accessors *)

  let access_field (member : member') t =
    match t with
    | StructV fields -> List.assoc member fields
    | _ ->
        Format.asprintf "Cannot access field %s of value: %a" member pp t
        |> failwith
end

(* Runtime representation of types *)

module Type = struct
  type t =
    | BoolT
    | AIntT
    | IntT of Bigint.t
    | BitT of Bigint.t
    | VBitT of Bigint.t
    | StrT
    | ErrT of member' list
    | MatchKindT of member' list
    | NameT of id'
    | NewT of id'
    | StackT of (t * Bigint.t)
    | TupleT of t list
    | StructT of (member' * t) list
    | HeaderT of (member' * t) list
    | UnionT of (member' * t) list
    (* (TODO) id' field of EnumT and SEnumT seems redundant,
       but also it may serve some purpose when type checking,
       e.g. enum foo { A, B } and enum bar { A, B } are different types *)
    | EnumT of id' * member' list
    | SEnumT of id' * t * (member' * Value.t) list
    | RefT

  let rec pp fmt = function
    | BoolT -> Format.fprintf fmt "bool"
    | AIntT -> Format.fprintf fmt "int"
    | IntT w -> Format.fprintf fmt "%ss" (Bigint.to_string w)
    | BitT w -> Format.fprintf fmt "%sw" (Bigint.to_string w)
    | VBitT w -> Format.fprintf fmt "%sv" (Bigint.to_string w)
    | StrT -> Format.fprintf fmt "string"
    | ErrT ms ->
        Format.fprintf fmt "error { @[<hv>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             Format.pp_print_string)
          ms
    | MatchKindT ms ->
        Format.fprintf fmt "match_kind { @[<hv>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             Format.pp_print_string)
          ms
    | NameT n -> Format.fprintf fmt "%s" n
    | NewT n -> Format.fprintf fmt "new %s" n
    | StackT (t, s) -> Format.fprintf fmt "%a[%s]" pp t (Bigint.to_string s)
    | TupleT ts ->
        Format.fprintf fmt "(%a)"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             pp)
          ts
    | StructT fs ->
        Format.fprintf fmt "struct { @[<hv>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (m, t) -> Format.fprintf fmt "%s: %a" m pp t))
          fs
    | HeaderT fs ->
        Format.fprintf fmt "header { @[<hv>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (m, t) -> Format.fprintf fmt "%s: %a" m pp t))
          fs
    | UnionT fs ->
        Format.fprintf fmt "union { @[<hv>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (m, t) -> Format.fprintf fmt "%s: %a" m pp t))
          fs
    | EnumT (_, ms) ->
        Format.fprintf fmt "enum { @[<hv>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             Format.pp_print_string)
          ms
    | SEnumT (_, t, fs) ->
        Format.fprintf fmt "enum %a { @[<hv>%a@] }" pp t
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (m, t) -> Format.fprintf fmt "%s: %a" m Value.pp t))
          fs
    | RefT -> Format.fprintf fmt "ref"
end

(* Type and value pairs *)

module TypeValue = struct
  type t = Type.t * Value.t

  let pp fmt (_, v) = Format.fprintf fmt "(%a)" Value.pp v
end

(* Runtime representation of functions *)

module Func = struct
  type t =
    | FuncF of {
        vis_glob : vis;
        tparams : tparam list;
        params : param list;
        ret : Type.t;
        body : block;
      }
    (* (TODO) Consider return type, which may be a type variable *)
    | ExternF of {
        vis_glob : vis;
        tparams : tparam list;
        params : param list; (* ret : Type.t; *)
      }
    | MethodF of {
        vis_obj : vis;
        tparams : tparam list;
        params : param list;
        body : block;
      }
    | ExternMethodF of {
        vis_obj : vis;
        tparams : tparam list;
        params : param list; (* ret : Type.t; *)
      }
    | StateF of { body : block }
    (* The visibility of an action depends on its declaration position *)
    | ActionF of { vis : vis; params : param list; body : block }
    | TableF of { vis_obj : vis }

  let pp fmt = function
    | FuncF _ -> Format.fprintf fmt "function"
    | ExternF _ -> Format.fprintf fmt "extern"
    | MethodF _ -> Format.fprintf fmt "method"
    | ExternMethodF _ -> Format.fprintf fmt "extern"
    | StateF _ -> Format.fprintf fmt "state"
    | ActionF _ -> Format.fprintf fmt "action"
    | TableF _ -> Format.fprintf fmt "table"

  (* Getters *)

  let get_params = function
    | FuncF { params; _ }
    | ExternF { params; _ }
    | MethodF { params; _ }
    | ExternMethodF { params; _ }
    | ActionF { params; _ } ->
        List.map (fun { it = id, _, _, _; _ } -> id.it) params
    | _ -> []
end

(* Environment of type variables, variables, and functions *)

module TDEnv = MakeEnv (Id) (Type)
module Env = MakeEnv (Id) (TypeValue)

module FEnv = struct
  include MakeEnv (FId) (Func)

  (* (TODO) resolve overloaded functions with argument names *)
  let find_opt (fid, args) fenv =
    let arity = List.length args in
    let funcs =
      List.filter
        (fun ((fid', params), _) -> fid = fid' && arity = List.length params)
        (bindings fenv)
    in
    assert (List.length funcs <= 1);
    match funcs with [] -> None | _ -> Some (List.hd funcs |> snd)

  let find (fid, args) fenv =
    match find_opt (fid, args) fenv with
    | Some f -> f
    | None -> Format.asprintf "Key not found: %s@." fid |> failwith
end

type env = TDEnv.t * Env.t * FEnv.t
type env_stack = TDEnv.t * Env.t list

let env_empty = (TDEnv.empty, Env.empty, FEnv.empty)
let env_stack_empty = (TDEnv.empty, [])

(* Transition between visibility and environment *)

let env_to_vis (env : env) =
  let tdenv, env, fenv = env in
  let tdvis =
    TDEnv.fold (fun tvar _ vis -> TDVis.add tvar vis) tdenv TDVis.empty
  in
  let vis = Env.fold (fun var _ vis -> Vis.add var vis) env Vis.empty in
  let fvis = FEnv.fold (fun fvar _ vis -> FVis.add fvar vis) fenv FVis.empty in
  (tdvis, vis, fvis)

let env_from_vis (env : env) (vis : vis) =
  let tdenv, env, fenv = env in
  let tdvis, vis, fvis = vis in
  let tdenv = TDEnv.filter (fun tvar _ -> TDVis.mem tvar tdvis) tdenv in
  let env = Env.filter (fun var _ -> Vis.mem var vis) env in
  let fenv = FEnv.filter (fun fvar _ -> FVis.mem fvar fvis) fenv in
  (tdenv, env, fenv)
