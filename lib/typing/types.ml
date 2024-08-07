open Syntax.Ast
open Runtime.Domain
module Value = Runtime.Value
open Util.Source

(* Types of variables *)

module rec Type : sig
  type t =
    (* Base types *)
    | VoidT
    | ErrT
    | MatchKindT
    | StrT
    | BoolT
    | AIntT
    | IntT of Bigint.t
    | BitT of Bigint.t
    | VBitT of Bigint.t
    (* Parameterized types *)
    (* Invariant: variables should always be bound *)
    | VarT of id'
    (* Alias types *)
    | DefT of t
    | NewT of t
    (* Aggregate types *)
    | TupleT of t list
    | StackT of t * Bigint.t
    | StructT of (member' * t) list
    | HeaderT of (member' * t) list
    | UnionT of (member' * t) list
    (* (TODO) maybe just id suffices *)
    | EnumT of member' list
    | SEnumT of t * (member' * Value.t) list
    (* Object types *)
    | ExternT of FDEnv.t
    | ParserT of FDEnv.t
    | ControlT of FDEnv.t
    | PackageT
    (* Top type *)
    | TopT

  val pp : Format.formatter -> t -> unit
end = struct
  type t =
    (* Base types *)
    | VoidT
    | ErrT
    | MatchKindT
    | StrT
    | BoolT
    | AIntT
    | IntT of Bigint.t
    | BitT of Bigint.t
    | VBitT of Bigint.t
    (* Parameterized types *)
    (* Variables should always be bound *)
    | VarT of id'
    (* Alias types *)
    | DefT of t
    | NewT of t
    (* Aggregate types *)
    | TupleT of t list
    | StackT of t * Bigint.t
    | StructT of (member' * t) list
    | HeaderT of (member' * t) list
    | UnionT of (member' * t) list
    (* (TODO) maybe just id suffices *)
    | EnumT of member' list
    | SEnumT of t * (member' * Value.t) list
    (* Object types *)
    | ExternT of FDEnv.t
    | ParserT of FDEnv.t
    | ControlT of FDEnv.t
    | PackageT
    (* Top type *)
    | TopT

  let rec pp fmt = function
    (* Base types *)
    | VoidT -> Format.fprintf fmt "void"
    | ErrT -> Format.fprintf fmt "error"
    | MatchKindT -> Format.fprintf fmt "match_kind"
    | StrT -> Format.fprintf fmt "string"
    | BoolT -> Format.fprintf fmt "bool"
    | AIntT -> Format.fprintf fmt "int"
    | IntT n -> Format.fprintf fmt "int<%a>" Bigint.pp n
    | BitT n -> Format.fprintf fmt "bit<%a>" Bigint.pp n
    | VBitT n -> Format.fprintf fmt "vbit<%a>" Bigint.pp n
    (* Parametrized types *)
    | VarT id -> Format.fprintf fmt "%s" id
    (* Alias types *)
    | DefT t -> Format.fprintf fmt "typedef %a" pp t
    | NewT t -> Format.fprintf fmt "type %a" pp t
    (* Aggregate types *)
    | TupleT ts ->
        Format.fprintf fmt "tuple<%a>"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             pp)
          ts
    | StackT (t, n) -> Format.fprintf fmt "stack %a[%a]" pp t Bigint.pp n
    | StructT fields ->
        Format.fprintf fmt "struct { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info) pp
                 t))
          fields
    | HeaderT fields ->
        Format.fprintf fmt "header { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info) pp
                 t))
          fields
    | UnionT fields ->
        Format.fprintf fmt "union { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info) pp
                 t))
          fields
    | EnumT members ->
        Format.fprintf fmt "enum { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt m ->
               Format.fprintf fmt "%a" Syntax.Pp.pp_member (m $ no_info)))
          members
    | SEnumT (t, members) ->
        Format.fprintf fmt "enum %a { %a }" pp t
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (m, v) ->
               Format.fprintf fmt "%a: %a" Syntax.Pp.pp_member (m $ no_info)
                 Runtime.Value.pp v))
          members
    (* Object types *)
    | ExternT fdenv -> Format.fprintf fmt "extern %a" FDEnv.pp fdenv
    | ParserT fdenv -> Format.fprintf fmt "parser %a" FDEnv.pp fdenv
    | ControlT fdenv -> Format.fprintf fmt "control %a" FDEnv.pp fdenv
    | PackageT -> Format.fprintf fmt "package"
    (* Top type *)
    | TopT -> Format.fprintf fmt "top"
end

and TypeDef : sig
  type t =
    (* Aliased type definitions *)
    | DefD of Type.t
    | NewD of Type.t
    (* Aggregate type definitions *)
    (* These will become generic in the future *)
    | StructD of (member' * Type.t) list
    | HeaderD of (member' * Type.t) list
    | UnionD of (member' * Type.t) list
    | EnumD of member' list
    | SEnumD of Type.t * (member' * Value.t) list
    (* Object type definitions *)
    | ExternD of tparam' list * FDEnv.t
    | ParserD of tparam' list * FDEnv.t
    | ControlD of tparam' list * FDEnv.t
    | PackageD of tparam' list

  val get_params : t -> tparam' list
  val pp : Format.formatter -> t -> unit
end = struct
  type t =
    (* Aliased type definitions *)
    | DefD of Type.t
    | NewD of Type.t
    (* Aggregate type definitions *)
    (* These will become generic in the future *)
    | StructD of (member' * Type.t) list
    | HeaderD of (member' * Type.t) list
    | UnionD of (member' * Type.t) list
    | EnumD of member' list
    | SEnumD of Type.t * (member' * Runtime.Value.t) list
    (* Object type definitions *)
    | ExternD of tparam' list * FDEnv.t
    | ParserD of tparam' list * FDEnv.t
    | ControlD of tparam' list * FDEnv.t
    | PackageD of tparam' list

  let get_params = function
    | DefD _ | NewD _ | StructD _ | HeaderD _ | UnionD _ | EnumD _ | SEnumD _ ->
        []
    | ExternD (params, _) -> params
    | ParserD (params, _) -> params
    | ControlD (params, _) -> params
    | PackageD params -> params

  let pp fmt = function
    | DefD typ -> Format.fprintf fmt "typedef %a" Type.pp typ
    | NewD typ -> Format.fprintf fmt "type %a" Type.pp typ
    | StructD fields ->
        Format.fprintf fmt "struct { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (member, typ) ->
               Format.fprintf fmt "%s: %a" member Type.pp typ))
          fields
    | HeaderD fields ->
        Format.fprintf fmt "header { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (member, typ) ->
               Format.fprintf fmt "%s: %a" member Type.pp typ))
          fields
    | UnionD fields ->
        Format.fprintf fmt "union { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (member, typ) ->
               Format.fprintf fmt "%s: %a" member Type.pp typ))
          fields
    | EnumD members ->
        Format.fprintf fmt "enum { %a }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt member -> Format.fprintf fmt "%s" member))
          members
    | SEnumD (typ, members) ->
        Format.fprintf fmt "enum %a { %a }" Type.pp typ
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             (fun fmt (member, value) ->
               Format.fprintf fmt "%s: %a" member Runtime.Value.pp value))
          members
    | ExternD (tparams, fdenv) ->
        Format.fprintf fmt "extern<%a> %a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt tparam -> Format.fprintf fmt "%s" tparam))
          tparams FDEnv.pp fdenv
    | ParserD (tparams, fdenv) ->
        Format.fprintf fmt "parser<%a> %a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt tparam -> Format.fprintf fmt "%s" tparam))
          tparams FDEnv.pp fdenv
    | ControlD (tparams, fdenv) ->
        Format.fprintf fmt "control<%a> %a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt tparam -> Format.fprintf fmt "%s" tparam))
          tparams FDEnv.pp fdenv
    | PackageD _tparams -> Format.fprintf fmt "package"
end

(* Types of functions *)
and FuncType : sig
  type t = (id' * dir' * Type.t * Value.t option) list * Type.t

  val pp : Format.formatter -> t -> unit
end = struct
  type t = (id' * dir' * Type.t * Value.t option) list * Type.t

  let pp fmt t =
    let params, typ_ret = t in
    Format.fprintf fmt "func (%a) -> %a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt (id, _dir, typ, _value_default) ->
           Format.fprintf fmt "%a %s" Type.pp typ id))
      params Type.pp typ_ret
end

and FuncDef : sig
  type t = tparam' list * (id' * dir' * Type.t * Value.t option) list * Type.t

  val pp : Format.formatter -> t -> unit
end = struct
  type t = tparam' list * (id' * dir' * Type.t * Value.t option) list * Type.t

  let pp fmt t =
    let tparams, params, typ_ret = t in
    Format.fprintf fmt "func<%a> (%a) -> %a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt tparam -> Format.fprintf fmt "%s" tparam))
      tparams
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt (id, _dir, typ, _value_default) ->
           Format.fprintf fmt "%a %s" Type.pp typ id))
      params Type.pp typ_ret
end

(* Types of constructors *)
and ConsType : sig
  type t = (id' * dir' * Type.t * Value.t option) list * Type.t

  val pp : Format.formatter -> t -> unit
end = struct
  type t = (id' * dir' * Type.t * Value.t option) list * Type.t

  let pp fmt t =
    let params, typ_ret = t in
    Format.fprintf fmt "cons (%a) -> %a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt (id, _dir, typ, _value_default) ->
           Format.fprintf fmt "%a %s" Type.pp typ id))
      params Type.pp typ_ret
end

and ConsDef : sig
  type t = tparam' list * (id' * dir' * Type.t * Value.t option) list * Type.t

  val pp : Format.formatter -> t -> unit
end = struct
  type t = tparam' list * (id' * dir' * Type.t * Value.t option) list * Type.t

  let pp fmt t =
    let tparams, params, typ_ret = t in
    Format.fprintf fmt "cons<%a> (%a) -> %a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt tparam -> Format.fprintf fmt "%s" tparam))
      tparams
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt (id, _dir, typ, _value_default) ->
           Format.fprintf fmt "%a %s" Type.pp typ id))
      params Type.pp typ_ret
end

(* Environments *)
and VEnv : (ENV with type t_key = Id.t and type t_value = Value.t) =
  MakeEnv (Id) (Value)

and TEnv : (ENV with type t_key = Id.t and type t_value = Type.t) =
  MakeEnv (Id) (Type)

and TDEnv : (ENV with type t_key = TId.t and type t_value = TypeDef.t) =
  MakeEnv (TId) (TypeDef)

and FDEnv : (ENV with type t_key = FId.t and type t_value = FuncDef.t) =
  MakeEnv (FId) (FuncDef)

and CDEnv : (ENV with type t_key = FId.t and type t_value = ConsDef.t) =
  MakeEnv (FId) (ConsDef)
