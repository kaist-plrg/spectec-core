open Syntax.Ast
open Runtime.Domain
module Value = Runtime.Value

(* Types of variables *)

module rec Type : sig
  type t_param = id' * dir' * t * Value.t option

  and t =
    (* Base types *)
    | VoidT
    | ErrT
    | MatchKindT
    | StrT
    | BoolT
    | IntT
    | FIntT of Bigint.t
    | FBitT of Bigint.t
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
    | ParserT of t_param list
    | ControlT of t_param list
    | PackageT
    (* Top type *)
    | TopT
    (* Synthesized types : variables can never be declared of this type *)
    | RecordT of (member' * t) list
    | SetT of t
    | StateT

  val pp : Format.formatter -> t -> unit
end = struct
  type t_param = id' * dir' * t * Value.t option

  and t =
    (* Base types *)
    | VoidT
    | ErrT
    | MatchKindT
    | StrT
    | BoolT
    | IntT
    | FIntT of Bigint.t
    | FBitT of Bigint.t
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
    | ParserT of t_param list
    | ControlT of t_param list
    | PackageT
    (* Top type *)
    | TopT
    (* Synthesized types : variables can never be declared of this type *)
    | RecordT of (member' * t) list
    | SetT of t
    | StateT

  let rec pp fmt t =
    let pp_params fmt params =
      Format.fprintf fmt "@[<v>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (id, _dir, typ, _value_default) ->
             Format.fprintf fmt "%a %s" pp typ id))
        params
    in
    match t with
    (* Base types *)
    | VoidT -> Format.fprintf fmt "void"
    | ErrT -> Format.fprintf fmt "error"
    | MatchKindT -> Format.fprintf fmt "match_kind"
    | StrT -> Format.fprintf fmt "string"
    | BoolT -> Format.fprintf fmt "bool"
    | IntT -> Format.fprintf fmt "int"
    | FIntT n -> Format.fprintf fmt "int<%a>" Bigint.pp n
    | FBitT n -> Format.fprintf fmt "bit<%a>" Bigint.pp n
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
        Format.fprintf fmt "struct { @[<v>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a"
                 (Syntax.Pp.pp_member' ~level:0)
                 m pp t))
          fields
    | HeaderT fields ->
        Format.fprintf fmt "header { @[<v>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a"
                 (Syntax.Pp.pp_member' ~level:0)
                 m pp t))
          fields
    | UnionT fields ->
        Format.fprintf fmt "union { @[<v>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a"
                 (Syntax.Pp.pp_member' ~level:0)
                 m pp t))
          fields
    | EnumT members ->
        Format.fprintf fmt "enum { @[<v>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt m ->
               Format.fprintf fmt "%a" (Syntax.Pp.pp_member' ~level:0) m))
          members
    | SEnumT (t, members) ->
        Format.fprintf fmt "enum %a { @[<v>%a@] }" pp t
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (m, v) ->
               Format.fprintf fmt "%a: %a"
                 (Syntax.Pp.pp_member' ~level:0)
                 m Runtime.Value.pp v))
          members
    (* Object types *)
    | ExternT fdenv -> Format.fprintf fmt "extern %a" FDEnv.pp fdenv
    | ParserT params -> Format.fprintf fmt "parser %a" pp_params params
    | ControlT params -> Format.fprintf fmt "control %a" pp_params params
    | PackageT -> Format.fprintf fmt "package"
    (* Top type *)
    | TopT -> Format.fprintf fmt "top"
    (* Synthesized types : variables can never be declared of this type *)
    | RecordT fields ->
        Format.fprintf fmt "record { @[<v>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (m, t) ->
               Format.fprintf fmt "%a: %a"
                 (Syntax.Pp.pp_member' ~level:0)
                 m pp t))
          fields
    | SetT t -> Format.fprintf fmt "set<%a>" pp t
    | StateT -> Format.fprintf fmt "state"
end

and TypeDef : sig
  type t_param = id' * dir' * Type.t * Value.t option

  type t =
    (* Aliased type definitions *)
    | DefD of Type.t
    | NewD of Type.t
    (* Aggregate type definitions *)
    (* These will become generic in the future *)
    | StructD of (member' * Type.t) list
    | HeaderD of (member' * Type.t) list
    | UnionD of (member' * Type.t) list
    | EnumD of id' * member' list
    | SEnumD of id' * Type.t * (member' * Value.t) list
    (* Object type definitions *)
    | ExternD of tparam' list * FDEnv.t
    | ParserD of tparam' list * t_param list
    | ControlD of tparam' list * t_param list
    | PackageD of tparam' list

  val pp : Format.formatter -> t -> unit
end = struct
  type t_param = id' * dir' * Type.t * Value.t option

  type t =
    (* Aliased type definitions *)
    | DefD of Type.t
    | NewD of Type.t
    (* Aggregate type definitions *)
    (* These will become generic in the future *)
    | StructD of (member' * Type.t) list
    | HeaderD of (member' * Type.t) list
    | UnionD of (member' * Type.t) list
    | EnumD of id' * member' list
    | SEnumD of id' * Type.t * (member' * Runtime.Value.t) list
    (* Object type definitions *)
    | ExternD of tparam' list * FDEnv.t
    | ParserD of tparam' list * t_param list
    | ControlD of tparam' list * t_param list
    | PackageD of tparam' list

  let pp fmt t =
    let pp_tparams fmt tparams =
      Format.fprintf fmt "<%a>"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt tparam -> Format.fprintf fmt "%s" tparam))
        tparams
    in
    let pp_params fmt params =
      Format.fprintf fmt "@[<v>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (id, _dir, typ, _value_default) ->
             Format.fprintf fmt "%a %s" Type.pp typ id))
        params
    in
    match t with
    | DefD typ -> Format.fprintf fmt "typedef %a" Type.pp typ
    | NewD typ -> Format.fprintf fmt "type %a" Type.pp typ
    | StructD fields ->
        Format.fprintf fmt "struct { @[<v>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (member, typ) ->
               Format.fprintf fmt "%s: %a" member Type.pp typ))
          fields
    | HeaderD fields ->
        Format.fprintf fmt "header { @[<v>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (member, typ) ->
               Format.fprintf fmt "%s: %a" member Type.pp typ))
          fields
    | UnionD fields ->
        Format.fprintf fmt "union { @[<v>%a@] }"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (member, typ) ->
               Format.fprintf fmt "%s: %a" member Type.pp typ))
          fields
    | EnumD (id, members) ->
        Format.fprintf fmt "enum %s { @[<v>%a@] }" id
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt member -> Format.fprintf fmt "%s" member))
          members
    | SEnumD (id, typ, members) ->
        Format.fprintf fmt "enum %a %s { @[<v>%a@] }" Type.pp typ id
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt (member, value) ->
               Format.fprintf fmt "%s: %a" member Runtime.Value.pp value))
          members
    | ExternD (tparams, fdenv) ->
        Format.fprintf fmt "extern%a %a" pp_tparams tparams FDEnv.pp fdenv
    | ParserD (tparams, params) ->
        Format.fprintf fmt "parser%a %a" pp_tparams tparams pp_params params
    | ControlD (tparams, params) ->
        Format.fprintf fmt "control%a %a" pp_tparams tparams pp_params params
    | PackageD _tparams -> Format.fprintf fmt "package"
end

(* Types of functions *)
and FuncType : sig
  type t_param = id' * dir' * Type.t * Value.t option

  type t =
    | ExternFunctionT of t_param list * Type.t
    | FunctionT of t_param list * Type.t
    | ActionT of t_param list
    | ExternMethodT of t_param list * Type.t
    | ExternAbstractMethodT of t_param list * Type.t
    | ParserApplyMethodT of t_param list
    | ControlApplyMethodT of t_param list
    | BuiltinMethodT of t_param list * Type.t

  val pp : Format.formatter -> t -> unit
  val get_params : t -> t_param list
  val get_typ_ret : t -> Type.t
end = struct
  type t_param = id' * dir' * Type.t * Value.t option

  type t =
    | ExternFunctionT of t_param list * Type.t
    | FunctionT of t_param list * Type.t
    | ActionT of t_param list
    | ExternMethodT of t_param list * Type.t
    | ExternAbstractMethodT of t_param list * Type.t
    | ParserApplyMethodT of t_param list
    | ControlApplyMethodT of t_param list
    | BuiltinMethodT of t_param list * Type.t

  let pp fmt t =
    let pp_params fmt params =
      Format.fprintf fmt "@[<v>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (id, _dir, typ, _value_default) ->
             Format.fprintf fmt "%a %s" Type.pp typ id))
        params
    in
    match t with
    | ExternFunctionT (params, typ_ret) ->
        Format.fprintf fmt "@[<v>extern func %a -> %a@]" pp_params params
          Type.pp typ_ret
    | FunctionT (params, typ_ret) ->
        Format.fprintf fmt "@[<v>func %a -> %a@]" pp_params params Type.pp
          typ_ret
    | ActionT params -> Format.fprintf fmt "@[<v>action %a@]" pp_params params
    | ExternMethodT (params, typ_ret) ->
        Format.fprintf fmt "@[<v>extern method %a -> %a@]" pp_params params
          Type.pp typ_ret
    | ExternAbstractMethodT (params, typ_ret) ->
        Format.fprintf fmt "@[<v>extern abstract method %a -> %a@]" pp_params
          params Type.pp typ_ret
    | ParserApplyMethodT params ->
        Format.fprintf fmt "@[<v>parser apply %a@]" pp_params params
    | ControlApplyMethodT params ->
        Format.fprintf fmt "@[<v>control apply %a@]" pp_params params
    | BuiltinMethodT (params, typ_ret) ->
        Format.fprintf fmt "@[<v>builtin method %a -> %a@]" pp_params params
          Type.pp typ_ret

  let get_params = function
    | ExternFunctionT (params, _)
    | FunctionT (params, _)
    | ActionT params
    | ExternMethodT (params, _)
    | ExternAbstractMethodT (params, _)
    | ParserApplyMethodT params
    | ControlApplyMethodT params
    | BuiltinMethodT (params, _) ->
        params

  let get_typ_ret = function
    | ExternFunctionT (_, typ_ret) | FunctionT (_, typ_ret) -> typ_ret
    | ActionT _ -> Type.VoidT
    | ExternMethodT (_, typ_ret) | ExternAbstractMethodT (_, typ_ret) -> typ_ret
    | ParserApplyMethodT _ | ControlApplyMethodT _ -> Type.VoidT
    | BuiltinMethodT (_, typ_ret) -> typ_ret
end

and FuncDef : sig
  type t_param = id' * dir' * Type.t * Value.t option

  type t =
    | ExternFunctionD of tparam' list * t_param list * Type.t
    | FunctionD of tparam' list * t_param list * Type.t
    | ActionD of t_param list
    | ExternMethodD of tparam' list * t_param list * Type.t
    | ExternAbstractMethodD of tparam' list * t_param list * Type.t

  val pp : Format.formatter -> t -> unit
end = struct
  type t_param = id' * dir' * Type.t * Value.t option

  type t =
    | ExternFunctionD of tparam' list * t_param list * Type.t
    | FunctionD of tparam' list * t_param list * Type.t
    | ActionD of t_param list
    | ExternMethodD of tparam' list * t_param list * Type.t
    | ExternAbstractMethodD of tparam' list * t_param list * Type.t

  let pp fmt t =
    let pp_tparams fmt tparams =
      Format.fprintf fmt "<%a>"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt tparam -> Format.fprintf fmt "%s" tparam))
        tparams
    in
    let pp_params fmt params =
      Format.fprintf fmt "@[<v>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (id, _dir, typ, _value_default) ->
             Format.fprintf fmt "%a %s" Type.pp typ id))
        params
    in
    match t with
    | ExternFunctionD (tparams, params, typ_ret) ->
        Format.fprintf fmt "@[<v>extern func%a %a -> %a@]" pp_tparams tparams
          pp_params params Type.pp typ_ret
    | FunctionD (tparams, params, typ_ret) ->
        Format.fprintf fmt "@[<v>func%a %a -> %a@]" pp_tparams tparams pp_params
          params Type.pp typ_ret
    | ActionD params -> Format.fprintf fmt "@[<v>action %a@]" pp_params params
    | ExternMethodD (tparams, params, typ_ret) ->
        Format.fprintf fmt "@[<v>extern method%a %a -> %a@]" pp_tparams tparams
          pp_params params Type.pp typ_ret
    | ExternAbstractMethodD (tparams, params, typ_ret) ->
        Format.fprintf fmt "@[<v>extern abstract method%a %a -> %a@]" pp_tparams
          tparams pp_params params Type.pp typ_ret
end

(* Types of constructors *)
and ConsType : sig
  type t = (id' * dir' * Type.t * Value.t option) list * Type.t

  val pp : Format.formatter -> t -> unit
end = struct
  type t = (id' * dir' * Type.t * Value.t option) list * Type.t

  let pp fmt t =
    let params, typ_ret = t in
    Format.fprintf fmt "@[<v>cons (@[<v>%a@]) -> %a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (id, _dir, typ, _value_default) ->
           Format.fprintf fmt "%a %s" Type.pp typ id))
      params Type.pp typ_ret
end

and ConsDef : sig
  type t = {
    tparams : tparam' list;
    cparams : (id' * dir' * Type.t * Value.t option) list;
    typ : Type.t;
  }

  val pp : Format.formatter -> t -> unit
end = struct
  type t = {
    tparams : tparam' list;
    cparams : (id' * dir' * Type.t * Value.t option) list;
    typ : Type.t;
  }

  let pp fmt t =
    Format.fprintf fmt "@[<v>cons<%a> (@[<v>%a@]) -> %a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt tparam -> Format.fprintf fmt "%s" tparam))
      t.tparams
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (id, _dir, typ, _value_default) ->
           Format.fprintf fmt "%a %s" Type.pp typ id))
      t.cparams Type.pp t.typ
end

(* Environments *)
and VEnv : (ENV with type t_key = Id.t and type t_value = Value.t) =
  MakeEnv (Id) (Value)

and TEnv : (ENV with type t_key = Id.t and type t_value = Type.t) =
  MakeEnv (Id) (Type)

and TDEnv : (ENV with type t_key = TId.t and type t_value = TypeDef.t) =
  MakeEnv (TId) (TypeDef)

and FDEnv : (FENV with type t_value = FuncDef.t) = struct
  include MakeEnv (FId) (FuncDef)

  (* (TODO) resolve overloaded functions with argument names *)
  let find_opt (fid, args) fdenv =
    let arity = List.length args in
    let fds =
      List.filter
        (fun ((fid', params), _) -> fid = fid' && arity = List.length params)
        (bindings fdenv)
    in
    assert (List.length fds <= 1);
    match fds with [] -> None | _ -> Some (List.hd fds |> snd)

  let find (fid, args) fdenv =
    match find_opt (fid, args) fdenv with
    | Some fd -> fd
    | None -> Format.asprintf "Key not found: %s@." fid |> failwith
end

and CDEnv : (FENV with type t_value = ConsDef.t) = struct
  include MakeEnv (FId) (ConsDef)

  (* (TODO) resolve overloaded functions with argument names *)
  let find_opt (cid, args) cdenv =
    let arity = List.length args in
    let cds =
      List.filter
        (fun ((cid', params), _) -> cid = cid' && arity = List.length params)
        (bindings cdenv)
    in
    assert (List.length cds <= 1);
    match cds with [] -> None | _ -> Some (List.hd cds |> snd)

  let find (cid, args) cdenv =
    match find_opt (cid, args) cdenv with
    | Some cd -> cd
    | None -> Format.asprintf "Key not found: %s@." cid |> failwith
end
