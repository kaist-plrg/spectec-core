open Syntax.Ast
open Domain

(* Visibility of type variables, variables, and function names *)

module TDVis = MakeVis (Var)
module Vis = MakeVis (Var)
module FVis = MakeVis (Var)

type vis_glob = TDVis.t * Vis.t * FVis.t
type vis_obj = vis_glob 

(* Runtime representation of values *)

module Value = struct
  type t =
    | BoolV of bool
    | AIntV of Bigint.t
    | IntV of Bigint.t * Bigint.t
    | BitV of Bigint.t * Bigint.t
    | VBitV of Bigint.t * Bigint.t
    | StrV of string
    | ErrV of string
    | TupleV of t list
    | StructV of (string * t) list
    | HeaderV of bool * (string * t) list
    | UnionV of (string * t) list
    | RefV of string list

  let rec pp fmt value =
    match value with
    | BoolV b -> Format.fprintf fmt "%b" b
    | AIntV i -> Format.fprintf fmt "%s" (Bigint.to_string i)
    | IntV (w, i) ->
        Format.fprintf fmt "%ss%s" (Bigint.to_string w) (Bigint.to_string i)
    | BitV (w, i) ->
        Format.fprintf fmt "%sw%s" (Bigint.to_string w) (Bigint.to_string i)
    | VBitV (w, i) ->
        Format.fprintf fmt "%sv%s" (Bigint.to_string w) (Bigint.to_string i)
    | StrV s -> Format.fprintf fmt "\"%s\"" s
    | ErrV s -> Format.fprintf fmt "%s" s
    | TupleV vs ->
        Format.fprintf fmt "(%a)"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             pp)
          vs
    | StructV fs ->
        Format.fprintf fmt "struct { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (f, v) -> Format.fprintf fmt "%s: %a" f pp v))
          fs
    | HeaderV (v, fs) ->
        Format.fprintf fmt "header { %s, %a }"
          (if v then "valid" else "invalid")
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (f, v) -> Format.fprintf fmt "%s: %a" f pp v))
          fs
    | UnionV fs ->
        Format.fprintf fmt "union { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (f, v) -> Format.fprintf fmt "%s: %a" f pp v))
          fs
    | RefV ps -> Format.fprintf fmt "ref %s" (String.concat "." ps)
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
    | ErrT
    | NameT of string
    | NewT of string
    | TupleT of t list
    | StructT of (string * t) list
    | HeaderT of (string * t) list
    | UnionT of (string * t) list
    | EnumT of string list
    | RefT

  let rec pp fmt typ =
    match typ with
    | BoolT -> Format.fprintf fmt "bool"
    | AIntT -> Format.fprintf fmt "int"
    | IntT w -> Format.fprintf fmt "%ss" (Bigint.to_string w)
    | BitT w -> Format.fprintf fmt "%sw" (Bigint.to_string w)
    | VBitT w -> Format.fprintf fmt "%sv" (Bigint.to_string w)
    | StrT -> Format.fprintf fmt "string"
    | ErrT -> Format.fprintf fmt "error"
    | NameT n -> Format.fprintf fmt "%s" n
    | NewT n -> Format.fprintf fmt "new %s" n
    | TupleT ts ->
        Format.fprintf fmt "(%a)"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             pp)
          ts
    | StructT fs ->
        Format.fprintf fmt "struct { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (f, t) -> Format.fprintf fmt "%s: %a" f pp t))
          fs
    | HeaderT fs ->
        Format.fprintf fmt "header { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (f, t) -> Format.fprintf fmt "%s: %a" f pp t))
          fs
    | UnionT fs ->
        Format.fprintf fmt "union { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (f, t) -> Format.fprintf fmt "%s: %a" f pp t))
          fs
    | EnumT ms ->
        Format.fprintf fmt "enum { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             Format.pp_print_string)
          ms
    | RefT -> Format.fprintf fmt "ref"
end

(* Type and value pairs *)

module TypeValue = struct
  type t = Type.t * Value.t

  let pp fmt (t, v) = Format.fprintf fmt "(%a, %a)" Type.pp t Value.pp v
end

(* Runtime representation of functions *)

module Func = struct
  type t =
    | FuncF of {
        vis_glob: vis_glob;
        tparams: string list;
        params: param list;
        ret: Type.t;
        body: block;
      }
    | MethodF of {
        vis_obj : vis_obj;
        tparams : string list;
        params : param list;
        body : block;
      }
    | StateF of {
        vis_obj : vis_obj;
        body : block;
      }
    | ActionF of {
        vis_obj : vis_obj;
        params : param list;
        body : block;
      }
    | TableF of { vis_obj : vis_obj }
    | ExternF

  let pp fmt = function
    | FuncF _ -> Format.fprintf fmt "function"
    | MethodF _ -> Format.fprintf fmt "method"
    | StateF _ -> Format.fprintf fmt "state"
    | ActionF _ -> Format.fprintf fmt "action"
    | TableF _ -> Format.fprintf fmt "table"
    | ExternF -> Format.fprintf fmt "extern"
end

(* Environment of type variables, variables, and functions *)

module TDEnv = MakeEnv (Var) (Type)
module Env = MakeEnv (Var) (TypeValue)
module FEnv = MakeEnv (Var) (Func)

type env_glob = TDEnv.t * Env.t * FEnv.t
type env_obj = env_glob
type env_loc = TDEnv.t * Env.t list
