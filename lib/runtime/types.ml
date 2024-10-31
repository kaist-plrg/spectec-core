open Domain
module L = Lang.Ast
module P = Lang.Pp
module E = Lang.Eq
module F = Format

(* Elaborated parameters *)

type tparam = L.tparam'

type param = L.id' * L.dir' * typ * Value.t option
and cparam = param

(* Types *)
and typ =
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
  | VarT of L.id'
  (* Alias types *)
  | NewT of L.id' * typ
  (* Constant types *)
  | EnumT of L.id' * L.member' list
  | SEnumT of L.id' * typ * (L.member' * Value.t) list
  (* Aggregate types *)
  | ListT of typ
  | TupleT of typ list
  | StackT of typ * Bigint.t
  | StructT of L.id' * (L.member' * typ) list
  | HeaderT of L.id' * (L.member' * typ) list
  | UnionT of L.id' * (L.member' * typ) list
  (* Object types *)
  | ExternT of L.id' * funcdef FIdMap.t
  | ParserT of param list
  | ControlT of param list
  | PackageT
  | TableT of typ
  (* Top type *)
  | TopT
  (* Synthesized types : variables can never be declared of this type *)
  | SeqT of typ list
  | RecordT of (L.member' * typ) list
  | InvalidT
  | SetT of typ
  | StateT

(* Type definitions *)
and typdef =
  (* Aliased type definitions *)
  | DefD of typ
  | NewD of L.id' * typ
  (* Constant type definitions *)
  | EnumD of L.id' * L.member' list
  | SEnumD of L.id' * typ * (L.member' * Value.t) list
  (* Aggregate type definitions *)
  (* These will become generic in the future *)
  | StructD of L.id' * (L.member' * typ) list
  | HeaderD of L.id' * (L.member' * typ) list
  | UnionD of L.id' * (L.member' * typ) list
  (* Object type definitions *)
  | ExternD of L.id' * tparam list * funcdef FIdMap.t
  | ParserD of tparam list * param list
  | ControlD of tparam list * param list
  | PackageD of tparam list

(* Function types *)
and functyp =
  | ActionT of param list
  | ExternFunctionT of param list * typ
  | FunctionT of param list * typ
  | ExternMethodT of param list * typ
  | ExternAbstractMethodT of param list * typ
  | ParserApplyMethodT of param list
  | ControlApplyMethodT of param list
  | BuiltinMethodT of param list * typ
  | TableApplyMethodT of typ

(* Function definitions *)
and funcdef =
  | ActionD of param list
  | ExternFunctionD of tparam list * param list * typ
  | FunctionD of tparam list * param list * typ
  | ExternMethodD of tparam list * param list * typ
  | ExternAbstractMethodD of tparam list * param list * typ
  | ParserApplyMethodD of param list
  | ControlApplyMethodD of param list

(* Constructor types *)

type constyp = param list * typ

(* Constructor definitions *)

type consdef = tparam list * param list * typ

(* Pretty-printers *)

(* Type parameters *)

let rec pp_tparam fmt tparam = P.pp_tparam' fmt tparam
and pp_tparams fmt tparams = P.pp_list pp_tparam ", " fmt tparams

(* Parameters *)

and pp_param fmt param =
  let id, dir, typ, value_default = param in
  match value_default with
  | Some value_default ->
      F.fprintf fmt "%a %a %a = %a" P.pp_dir' dir P.pp_id' id pp_typ typ
        Value.pp value_default
  | None -> F.fprintf fmt "%a %a %a" P.pp_dir' dir P.pp_id' id pp_typ typ

and pp_params fmt params = P.pp_list pp_param ", " fmt params

(* Constructor parameters *)

and pp_cparam fmt cparam = pp_param fmt cparam
and pp_cparams fmt cparams = P.pp_list pp_cparam ", " fmt cparams

(* Types *)

and pp_typ fmt typ =
  match typ with
  | VoidT -> F.pp_print_string fmt "void"
  | ErrT -> F.pp_print_string fmt "error"
  | MatchKindT -> F.pp_print_string fmt "match_kind"
  | StrT -> F.pp_print_string fmt "string"
  | BoolT -> F.pp_print_string fmt "bool"
  | IntT -> F.pp_print_string fmt "int"
  | FIntT width -> F.fprintf fmt "int<%a>" Bigint.pp width
  | FBitT width -> F.fprintf fmt "bit<%a>" Bigint.pp width
  | VBitT width -> F.fprintf fmt "varbit<%a>" Bigint.pp width
  | VarT id -> P.pp_id' fmt id
  | NewT (id, typ) -> F.fprintf fmt "type %a (= %a)" P.pp_id' id pp_typ typ
  | EnumT (id, _) -> F.fprintf fmt "enum %a" P.pp_id' id
  | SEnumT (id, typ, _) -> F.fprintf fmt "enum<%a> %a" pp_typ typ P.pp_id' id
  | ListT typ -> F.fprintf fmt "list<%a>" pp_typ typ
  | TupleT typs -> F.fprintf fmt "tuple<%a>" (P.pp_list pp_typ ",@ ") typs
  | StackT (typ, size) -> F.fprintf fmt "%a[%a]" pp_typ typ Bigint.pp size
  | StructT (id, fields) ->
      F.fprintf fmt "struct %a { %a }" P.pp_id' id
        (P.pp_pairs P.pp_member' pp_typ "; ")
        fields
  | HeaderT (id, fields) ->
      F.fprintf fmt "header %a { %a }" P.pp_id' id
        (P.pp_pairs P.pp_member' pp_typ "; ")
        fields
  | UnionT (id, fields) ->
      F.fprintf fmt "header_union %a { %a }" P.pp_id' id
        (P.pp_pairs P.pp_member' pp_typ "; ")
        fields
  | ExternT (id, fdenv) ->
      F.fprintf fmt "extern %a %a" P.pp_id' id (FIdMap.pp pp_funcdef) fdenv
  | ParserT params -> F.fprintf fmt "parser (%a)" pp_params params
  | ControlT params -> F.fprintf fmt "control (%a)" pp_params params
  | PackageT -> F.fprintf fmt "package"
  | TableT _ -> F.pp_print_string fmt "table"
  | TopT -> F.pp_print_string fmt "top"
  | SeqT typs -> F.fprintf fmt "seq<%a>" (P.pp_list pp_typ ",@ ") typs
  | RecordT fields ->
      F.fprintf fmt "record { %a }" (P.pp_pairs P.pp_member' pp_typ "; ") fields
  | InvalidT -> F.pp_print_string fmt "{#}"
  | SetT typ -> F.fprintf fmt "set<%a>" pp_typ typ
  | StateT -> F.pp_print_string fmt "state"

(* Type definitions *)

and pp_typdef fmt typdef =
  match typdef with
  | DefD typ -> F.fprintf fmt "typedef %a" pp_typ typ
  | NewD (id, typ) -> F.fprintf fmt "type %a (= %a)" P.pp_id' id pp_typ typ
  | EnumD (id, members) ->
      F.fprintf fmt "enum %a { %a }" P.pp_id' id
        (P.pp_list P.pp_member' ", ")
        members
  | SEnumD (id, typ, fields) ->
      F.fprintf fmt "enum<%a> %a { %a }" pp_typ typ P.pp_id' id
        (P.pp_pairs P.pp_member' Value.pp ", ")
        fields
  | StructD (id, fields) ->
      F.fprintf fmt "struct %a { %a }" P.pp_id' id
        (P.pp_pairs P.pp_member' pp_typ "; ")
        fields
  | HeaderD (id, fields) ->
      F.fprintf fmt "header %a { %a }" P.pp_id' id
        (P.pp_pairs P.pp_member' pp_typ "; ")
        fields
  | UnionD (id, fields) ->
      F.fprintf fmt "header_union %a { %a }" P.pp_id' id
        (P.pp_pairs P.pp_member' pp_typ "; ")
        fields
  | ExternD (id, tparams, fdenv) ->
      F.fprintf fmt "extern %a<%a> %a" pp_tparams tparams P.pp_id' id
        (FIdMap.pp pp_funcdef) fdenv
  | ParserD (tparams, params) ->
      F.fprintf fmt "parser <%a>(%a)" pp_tparams tparams pp_params params
  | ControlD (tparams, params) ->
      F.fprintf fmt "control <%a>(%a)" pp_tparams tparams pp_params params
  | PackageD tparams -> F.fprintf fmt "package <%a>" pp_tparams tparams

(* Function types *)

and pp_functyp fmt functyp =
  match functyp with
  | ActionT params -> F.fprintf fmt "action(%a)" pp_params params
  | ExternFunctionT (params, typ) ->
      F.fprintf fmt "extern_func(%a) -> %a" pp_params params pp_typ typ
  | FunctionT (params, typ) ->
      F.fprintf fmt "func(%a) -> %a" pp_params params pp_typ typ
  | ExternMethodT (params, typ) ->
      F.fprintf fmt "extern_method(%a) -> %a" pp_params params pp_typ typ
  | ExternAbstractMethodT (params, typ) ->
      F.fprintf fmt "extern_abstract_method(%a) -> %a" pp_params params pp_typ
        typ
  | ParserApplyMethodT params ->
      F.fprintf fmt "parser_apply(%a)" pp_params params
  | ControlApplyMethodT params ->
      F.fprintf fmt "control_apply(%a)" pp_params params
  | BuiltinMethodT (params, typ) ->
      F.fprintf fmt "builtin_method(%a) -> %a" pp_params params pp_typ typ
  | TableApplyMethodT _ -> F.fprintf fmt "table_apply"

(* Function definitions *)

and pp_funcdef fmt funcdef =
  match funcdef with
  | ActionD params -> F.fprintf fmt "action(%a)" pp_params params
  | ExternFunctionD (tparams, params, typ) ->
      F.fprintf fmt "extern_func<%a>(%a) -> %a" pp_tparams tparams pp_params
        params pp_typ typ
  | FunctionD (tparams, params, typ) ->
      F.fprintf fmt "func<%a>(%a) -> %a" pp_tparams tparams pp_params params
        pp_typ typ
  | ExternMethodD (tparams, params, typ) ->
      F.fprintf fmt "extern_method<%a>(%a) -> %a" pp_tparams tparams pp_params
        params pp_typ typ
  | ExternAbstractMethodD (tparams, params, typ) ->
      F.fprintf fmt "extern_abstract_method<%a>(%a) -> %a" pp_tparams tparams
        pp_params params pp_typ typ
  | ParserApplyMethodD params ->
      F.fprintf fmt "parser_apply(%a)" pp_params params
  | ControlApplyMethodD params ->
      F.fprintf fmt "control_apply(%a)" pp_params params

(* Constructor types *)

let pp_constyp fmt constyp =
  let cparams, typ = constyp in
  F.fprintf fmt "constructor (%a) -> %a" pp_cparams cparams pp_typ typ

(* Constructor definitions *)

let pp_consdef fmt consdef =
  let tparams, cparams, typ = consdef in
  F.fprintf fmt "constructor<%a> (%a) -> %a" pp_tparams tparams pp_cparams
    cparams pp_typ typ

(* Equality *)

(* Type parameters *)

let rec eq_tparam tparam_a tparam_b = E.eq_tparam' tparam_a tparam_b
and eq_tparams tparams_a tparams_b = E.eq_list eq_tparam tparams_a tparams_b

(* Parameters *)

and eq_param param_a param_b =
  let id_a, dir_a, typ_a, value_default_a = param_a in
  let id_b, dir_b, typ_b, value_default_b = param_b in
  E.eq_id' id_a id_b && E.eq_dir' dir_a dir_b && eq_typ typ_a typ_b
  && E.eq_option Value.eq value_default_a value_default_b

and eq_params params_a params_b = E.eq_list eq_param params_a params_b

(* Constructor parameters *)

and eq_cparam cparam_a cparam_b = eq_param cparam_a cparam_b
and eq_cparams cparams_a cparams_b = E.eq_list eq_cparam cparams_a cparams_b

(* Types *)

and eq_typ typ_a typ_b =
  match (typ_a, typ_b) with
  | VoidT, VoidT
  | ErrT, ErrT
  | MatchKindT, MatchKindT
  | StrT, StrT
  | BoolT, BoolT
  | IntT, IntT ->
      true
  | FIntT width_a, FIntT width_b
  | FBitT width_a, FBitT width_b
  | VBitT width_a, VBitT width_b ->
      Bigint.(width_a = width_b)
  | VarT id_a, VarT id_b -> E.eq_id' id_a id_b
  | NewT (id_a, typ_a), NewT (id_b, typ_b) ->
      E.eq_id' id_a id_b && eq_typ typ_a typ_b
  | EnumT (id_a, members_a), EnumT (id_b, members_b) ->
      E.eq_id' id_a id_b && List.for_all2 E.eq_member' members_a members_b
  | SEnumT (id_a, typ_a, fields_a), SEnumT (id_b, typ_b, fields_b) ->
      E.eq_id' id_a id_b && eq_typ typ_a typ_b
      && List.for_all2
           (fun (member_a, value_a) (member_b, value_b) ->
             E.eq_member' member_a member_b && Value.eq value_a value_b)
           fields_a fields_b
  | ListT typ_a, ListT typ_b -> eq_typ typ_a typ_b
  | TupleT typs_a, TupleT typs_b -> List.for_all2 eq_typ typs_a typs_b
  | StackT (typ_a, size_a), StackT (typ_b, size_b) ->
      eq_typ typ_a typ_b && Bigint.(size_a = size_b)
  | StructT (id_a, fields_a), StructT (id_b, fields_b)
  | HeaderT (id_a, fields_a), HeaderT (id_b, fields_b)
  | UnionT (id_a, fields_a), UnionT (id_b, fields_b) ->
      E.eq_id' id_a id_b && E.eq_pairs E.eq_member' eq_typ fields_a fields_b
  | ExternT (id_a, fdenv_a), ExternT (id_b, fdenv_b) ->
      E.eq_id' id_a id_b && FIdMap.eq eq_funcdef fdenv_a fdenv_b
  | ParserT params_a, ParserT params_b | ControlT params_a, ControlT params_b ->
      List.for_all2 eq_param params_a params_b
  | PackageT, PackageT | TopT, TopT -> true
  | SeqT typs_a, SeqT typs_b -> List.for_all2 eq_typ typs_a typs_b
  | RecordT fields_a, RecordT fields_b ->
      E.eq_pairs E.eq_member' eq_typ fields_a fields_b
  | InvalidT, InvalidT -> true
  | SetT typ_a, SetT typ_b -> eq_typ typ_a typ_b
  | StateT, StateT -> true
  | _ -> false

(* Function definitions *)

and eq_funcdef funcdef_a funcdef_b =
  match (funcdef_a, funcdef_b) with
  | ActionD params_a, ActionD params_b -> eq_params params_a params_b
  | ( ExternFunctionD (tparams_a, params_a, typ_a),
      ExternFunctionD (tparams_b, params_b, typ_b) )
  | ( FunctionD (tparams_a, params_a, typ_a),
      FunctionD (tparams_b, params_b, typ_b) )
  | ( ExternMethodD (tparams_a, params_a, typ_a),
      ExternMethodD (tparams_b, params_b, typ_b) )
  | ( ExternAbstractMethodD (tparams_a, params_a, typ_a),
      ExternAbstractMethodD (tparams_b, params_b, typ_b) ) ->
      eq_tparams tparams_a tparams_b
      && eq_params params_a params_b
      && eq_typ typ_a typ_b
  | ParserApplyMethodD params_a, ParserApplyMethodD params_b
  | ControlApplyMethodD params_a, ControlApplyMethodD params_b ->
      eq_params params_a params_b
  | _ -> false

(* Modules *)

module Type = struct
  type t = typ

  let pp = pp_typ
  let eq = eq_typ

  let rec is_numeric typ =
    match typ with
    | IntT | FIntT _ | FBitT _ -> true
    | SEnumT (_, typ_inner, _) -> is_numeric typ_inner
    | _ -> false

  let rec is_ground typ =
    match typ with
    | VoidT | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _
    | VBitT _ ->
        true
    | VarT _ -> false
    | NewT (_, typ_inner) -> is_ground typ_inner
    | EnumT _ -> true
    | SEnumT (_, typ_inner, _) -> is_ground typ_inner
    | ListT typ_inner -> is_ground typ_inner
    | TupleT typs_inner -> List.for_all is_ground typs_inner
    | StackT (typ_inner, _) -> is_ground typ_inner
    | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
        List.map snd fields |> List.for_all is_ground
    | ExternT _ | ParserT _ | ControlT _ | PackageT | TopT -> true
    | SeqT typs_inner -> List.for_all is_ground typs_inner
    | RecordT fields -> List.map snd fields |> List.for_all is_ground
    | InvalidT -> true
    | SetT typ_inner -> is_ground typ_inner
    | StateT -> true
    | TableT _ -> true

  let rec can_equals typ =
    match typ with
    | VoidT -> false
    | ErrT | MatchKindT | StrT | BoolT | IntT | FIntT _ | FBitT _ | VBitT _ ->
        true
    | VarT _ -> false
    | NewT (_, typ_inner) -> can_equals typ_inner
    | EnumT _ -> true
    | SEnumT (_, typ_inner, _) | ListT typ_inner -> can_equals typ_inner
    | TupleT typs_inner -> List.for_all can_equals typs_inner
    | StackT (typ_inner, _) -> can_equals typ_inner
    | StructT (_, fields) | HeaderT (_, fields) | UnionT (_, fields) ->
        List.map snd fields |> List.for_all can_equals
    | ExternT _ | ParserT _ | ControlT _ | PackageT | TopT -> false
    | SeqT typs_inner -> List.for_all can_equals typs_inner
    | RecordT fields -> List.map snd fields |> List.for_all can_equals
    | InvalidT -> true
    | SetT _ | StateT | TableT _ -> false
end

module TypeDef = struct
  type t = typdef

  let pp = pp_typdef
end

module FuncType = struct
  type t = functyp

  let pp = pp_functyp
  let is_action = function ActionT _ -> true | _ -> false

  let get_params = function
    | ActionT params
    | ExternFunctionT (params, _)
    | FunctionT (params, _)
    | ExternMethodT (params, _)
    | ExternAbstractMethodT (params, _)
    | ParserApplyMethodT params
    | ControlApplyMethodT params
    | BuiltinMethodT (params, _) ->
        params
    | TableApplyMethodT _ -> []

  let get_typ_ret = function
    | ActionT _ -> VoidT
    | ExternFunctionT (_, typ_ret)
    | FunctionT (_, typ_ret)
    | ExternMethodT (_, typ_ret)
    | ExternAbstractMethodT (_, typ_ret) ->
        typ_ret
    | ParserApplyMethodT _ | ControlApplyMethodT _ -> VoidT
    | TableApplyMethodT typ_ret -> typ_ret
    | BuiltinMethodT (_, typ_ret) -> typ_ret
end

module FuncDef = struct
  type t = funcdef

  let pp = pp_funcdef
  let eq = eq_funcdef

  let get_params = function
    | ActionD params
    | ExternFunctionD (_, params, _)
    | FunctionD (_, params, _)
    | ExternMethodD (_, params, _)
    | ExternAbstractMethodD (_, params, _)
    | ParserApplyMethodD params
    | ControlApplyMethodD params ->
        params

  let get_typ_ret = function
    | ActionD _ -> VoidT
    | ExternFunctionD (_, _, typ_ret) | FunctionD (_, _, typ_ret) -> typ_ret
    | ExternMethodD (_, _, typ_ret) | ExternAbstractMethodD (_, _, typ_ret) ->
        typ_ret
    | ParserApplyMethodD _ | ControlApplyMethodD _ -> VoidT
end

module ConsType = struct
  type t = constyp

  let pp = pp_constyp
end

module ConsDef = struct
  type t = consdef

  let pp = pp_consdef
end
