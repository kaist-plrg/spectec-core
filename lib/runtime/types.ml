open Domain
module L = Lang.Ast
module P = Lang.Pp
module F = Format

(* Elaborated parameters *)

type param' = (typ, Value.t, Dir.t) L.param'

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
  | DefT of typ
  | NewT of typ
  (* Aggregate types *)
  | TupleT of typ list
  | StackT of typ * Bigint.t
  | StructT of (L.member' * typ) list
  | HeaderT of (L.member' * typ) list
  | UnionT of (L.member' * typ) list
  (* (TODO) maybe just id suffices *)
  | EnumT of L.member' list
  | SEnumT of typ * (L.member' * Value.t) list
  (* Object types *)
  | ExternT of funcdef FIdMap.t
  | ParserT of param' list
  | ControlT of param' list
  | PackageT
  (* Top type *)
  | TopT
  (* Synthesized types : variables can never be declared of this type *)
  | RecordT of (L.member' * typ) list
  | SetT of typ
  | StateT

(* Type definitions *)
and typdef =
  (* Aliased type definitions *)
  | DefD of typ
  | NewD of typ
  (* Aggregate type definitions *)
  (* These will become generic in the future *)
  | StructD of (L.member' * typ) list
  | HeaderD of (L.member' * typ) list
  | UnionD of (L.member' * typ) list
  | EnumD of L.id' * L.member' list
  | SEnumD of L.id' * typ * (L.member' * Value.t) list
  (* Object type definitions *)
  | ExternD of L.tparam' list * funcdef FIdMap.t
  | ParserD of L.tparam' list * param' list
  | ControlD of L.tparam' list * param' list
  | PackageD of L.tparam' list

(* Function types *)
and functyp =
  | ExternFunctionT of param' list * typ
  | FunctionT of param' list * typ
  | ActionT of param' list
  | ExternMethodT of param' list * typ
  | ExternAbstractMethodT of param' list * typ
  | ParserApplyMethodT of param' list
  | ControlApplyMethodT of param' list
  | BuiltinMethodT of param' list * typ

(* Function definitions *)
and funcdef =
  | ExternFunctionD of L.tparam' list * param' list * typ
  | FunctionD of L.tparam' list * param' list * typ
  | ActionD of param' list
  | ExternMethodD of L.tparam' list * param' list * typ
  | ExternAbstractMethodD of L.tparam' list * param' list * typ

(* Constructor types *)

type constyp = param' list * typ

(* Constructor definitions *)

type consdef = L.tparam' list * param' list * typ

(* Pretty-printers *)

(* Types *)

let rec pp_typ fmt typ =
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
  | DefT typ -> F.fprintf fmt "typedef<%a>" pp_typ typ
  | NewT typ -> F.fprintf fmt "type<%a>" pp_typ typ
  | TupleT typs -> F.fprintf fmt "tuple<%a>" (P.pp_list pp_typ ",@ ") typs
  | StackT (typ, size) -> F.fprintf fmt "%a[%a]" pp_typ typ Bigint.pp size
  | StructT fields ->
      F.fprintf fmt "struct { %a }" (P.pp_pairs P.pp_member' pp_typ "; ") fields
  | HeaderT fields ->
      F.fprintf fmt "header { %a }" (P.pp_pairs P.pp_member' pp_typ "; ") fields
  | UnionT fields ->
      F.fprintf fmt "header_union { %a }"
        (P.pp_pairs P.pp_member' pp_typ "; ")
        fields
  | EnumT members ->
      F.fprintf fmt "enum { %a }" (P.pp_list P.pp_member' ",@ ") members
  | SEnumT (typ, fields) ->
      F.fprintf fmt "enum<%a> { %a }" pp_typ typ
        (P.pp_pairs P.pp_member' Value.pp "; ")
        fields
  | ExternT fdenv -> F.fprintf fmt "extern %a" (FIdMap.pp pp_funcdef) fdenv
  | ParserT params ->
      let param = List.hd params in
      F.fprintf fmt "parser(%a)" pp_param' param
  | ControlT params -> F.fprintf fmt "control(%a)" pp_params' params
  | PackageT -> F.pp_print_string fmt "package"
  | TopT -> F.pp_print_string fmt "top"
  | RecordT fields ->
      F.fprintf fmt "record { %a }" (P.pp_pairs P.pp_member' pp_typ "; ") fields
  | SetT typ -> F.fprintf fmt "set<%a>" pp_typ typ
  | StateT -> F.pp_print_string fmt "state"

(* Type parameters *)

and pp_tparam' fmt tparam = P.pp_tparam' fmt tparam
and pp_tparams' fmt tparams = P.pp_list pp_tparam' ", " fmt tparams

(* Parameters *)

and pp_param' fmt param = P.pp_param' pp_typ Value.pp Dir.pp fmt param
and pp_params' fmt _params = F.pp_print_string fmt "TODO"

(* Type definitions *)

and pp_typdef fmt typdef =
  match typdef with
  | DefD typ -> F.fprintf fmt "typedef<%a>" pp_typ typ
  | NewD typ -> F.fprintf fmt "type<%a>" pp_typ typ
  | StructD fields ->
      F.fprintf fmt "struct { %a }" (P.pp_pairs P.pp_member' pp_typ "; ") fields
  | HeaderD fields ->
      F.fprintf fmt "header { %a }" (P.pp_pairs P.pp_member' pp_typ "; ") fields
  | UnionD fields ->
      F.fprintf fmt "header_union { %a }"
        (P.pp_pairs P.pp_member' pp_typ "; ")
        fields
  | EnumD (id, members) ->
      F.fprintf fmt "enum %a { %a }" P.pp_id' id
        (P.pp_list P.pp_member' ", ")
        members
  | SEnumD (id, typ, fields) ->
      F.fprintf fmt "enum<%a> %a { %a }" pp_typ typ P.pp_id' id
        (P.pp_pairs P.pp_member' Value.pp ", ")
        fields
  | ExternD (tparams, fdenv) ->
      F.fprintf fmt "extern<%a> %a"
        (P.pp_list P.pp_tparam' ", ")
        tparams (FIdMap.pp pp_funcdef) fdenv
  | ParserD (tparams, params) ->
      F.fprintf fmt "parser<%a>(%a)" pp_tparams' tparams pp_params' params
  | ControlD (tparams, params) ->
      F.fprintf fmt "control<%a>(%a)" pp_tparams' tparams pp_params' params
  | PackageD tparams -> F.fprintf fmt "package<%a>" pp_tparams' tparams

(* Function types *)

and pp_functyp fmt functyp =
  match functyp with
  | ExternFunctionT (params, typ) ->
      F.fprintf fmt "extern_func(%a) -> %a" pp_params' params pp_typ typ
  | FunctionT (params, typ) ->
      F.fprintf fmt "func(%a) -> %a" pp_params' params pp_typ typ
  | ActionT params -> F.fprintf fmt "action(%a)" pp_params' params
  | ExternMethodT (params, typ) ->
      F.fprintf fmt "extern_method(%a) -> %a" pp_params' params pp_typ typ
  | ExternAbstractMethodT (params, typ) ->
      F.fprintf fmt "extern_abstract_method(%a) -> %a" pp_params' params pp_typ
        typ
  | ParserApplyMethodT params ->
      F.fprintf fmt "parser_apply(%a)" pp_params' params
  | ControlApplyMethodT params ->
      F.fprintf fmt "control_apply(%a)" pp_params' params
  | BuiltinMethodT (params, typ) ->
      F.fprintf fmt "builtin_method(%a) -> %a" pp_params' params pp_typ typ

(* Function definitions *)

and pp_funcdef fmt funcdef =
  match funcdef with
  | ExternFunctionD (tparams, params, typ) ->
      F.fprintf fmt "extern_func<%a>(%a) -> %a" pp_tparams' tparams pp_params'
        params pp_typ typ
  | FunctionD (tparams, params, typ) ->
      F.fprintf fmt "func<%a>(%a) -> %a" pp_tparams' tparams pp_params' params
        pp_typ typ
  | ActionD params -> F.fprintf fmt "action(%a)" pp_params' params
  | ExternMethodD (tparams, params, typ) ->
      F.fprintf fmt "extern_method<%a>(%a) -> %a" pp_tparams' tparams pp_params'
        params pp_typ typ
  | ExternAbstractMethodD (tparams, params, typ) ->
      F.fprintf fmt "extern_abstract_method<%a>(%a) -> %a" pp_tparams' tparams
        pp_params' params pp_typ typ

(* Constructor types *)

let pp_constyp fmt constyp =
  let params, typ = constyp in
  F.fprintf fmt "constructor (%a) -> %a" pp_params' params pp_typ typ

(* Constructor definitions *)

let pp_consdef fmt consdef =
  let tparams, params, typ = consdef in
  F.fprintf fmt "constructor<%a> (%a) -> %a" pp_tparams' tparams pp_params'
    params pp_typ typ

(* Modules *)

module Type = struct
  type t = typ

  let pp = pp_typ
end

module TypeDef = struct
  type t = typdef

  let pp = pp_typdef
end

module FuncType = struct
  type t = functyp

  let pp = pp_functyp
end

module FuncDef = struct
  type t = funcdef

  let pp = pp_funcdef
end

module ConsType = struct
  type t = constyp

  let pp = pp_constyp
end

module ConsDef = struct
  type t = consdef

  let pp = pp_consdef
end
