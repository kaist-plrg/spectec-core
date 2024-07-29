open Syntax.Ast
open Util.Source

module BaseType = struct
  type t =
    | VoidT
    | ErrT
    | MatchKindT
    | StrT
    | BoolT
    | AIntT
    | IntT of Bigint.t
    | BitT of Bigint.t
    | VBitT of Bigint.t
    | TupleT of t list
    | StackT of t * Bigint.t
    | VarT of var'
    | SVarT of var' * t list

  let rec pp fmt = function
    | VoidT -> Format.fprintf fmt "void"
    | ErrT -> Format.fprintf fmt "error"
    | MatchKindT -> Format.fprintf fmt "match_kind"
    | StrT -> Format.fprintf fmt "string"
    | BoolT -> Format.fprintf fmt "bool"
    | AIntT -> Format.fprintf fmt "int"
    | IntT n -> Format.fprintf fmt "int<%a>" Bigint.pp n
    | BitT n -> Format.fprintf fmt "bit<%a>" Bigint.pp n
    | VBitT n -> Format.fprintf fmt "vbit<%a>" Bigint.pp n
    | TupleT ts ->
        Format.fprintf fmt "tuple<%a>"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             pp)
          ts
    | StackT (t, n) -> Format.fprintf fmt "stack %a[%a]" pp t Bigint.pp n
    | VarT var -> Format.fprintf fmt "%a" Syntax.Pp.pp_var (var $ no_info)
    | SVarT (var, ts) ->
        Format.fprintf fmt "%a<%a>" Syntax.Pp.pp_var (var $ no_info)
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             pp)
          ts
end

module FuncType = struct
  type t = BaseType.t list * BaseType.t

  let pp fmt _t = Format.fprintf fmt "functype"
end

module ConsType = struct
  type t = BaseType.t list * BaseType.t

  let pp fmt _t = Format.fprintf fmt "constype"
end

module TypeDef = struct
  type t =
    (* Aliased type definitions *)
    | DefT of BaseType.t
    | NewT of BaseType.t
    (* Aggregate type definitions *)
    | StructT of (member' * BaseType.t) list
    | HeaderT of (member' * BaseType.t) list
    | UnionT of (member' * BaseType.t) list
    | EnumT of member' list
    | SEnumT of BaseType.t * (member' * Runtime.Value.t) list
    (* Object type definitions *)
    | ExternT of tparam' list
    | ParserT of tparam' list * param' list
    | ControlT of tparam' list * param' list
    | PackageT of tparam' list * param' list

  let pp fmt = function
    | DefT t -> Format.fprintf fmt "typedef %a" BaseType.pp t
    | NewT t -> Format.fprintf fmt "type %a" BaseType.pp t
    | StructT fields ->
        Format.fprintf fmt "struct { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info)
                 BaseType.pp t))
          fields
    | HeaderT fields ->
        Format.fprintf fmt "header { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info)
                 BaseType.pp t))
          fields
    | UnionT fields ->
        Format.fprintf fmt "union { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info)
                 BaseType.pp t))
          fields
    | EnumT members ->
        Format.fprintf fmt "enum { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt m ->
               Format.fprintf fmt "%a" Syntax.Pp.pp_member (m $ no_info)))
          members
    | SEnumT (t, members) ->
        Format.fprintf fmt "enum %a { %a }" BaseType.pp t
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, v) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info)
                 Runtime.Value.pp v))
          members
    | ExternT _ -> Format.fprintf fmt "extern"
    | ParserT _ -> Format.fprintf fmt "parser"
    | ControlT _ -> Format.fprintf fmt "control"
    | PackageT _ -> Format.fprintf fmt "package"
end
