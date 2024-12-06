open Domain.Dom
open Tdom
module P = Lang.Pp

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
  | SpecT (td, typs) -> (
      match td with
      | MonoD typ ->
          assert (typs = []);
          pp_typ fmt typ
      | PolyD (tparams, tparams_hidden, typ) ->
          F.fprintf fmt "(%a<%a @@ %a>)<%a>" pp_typ typ pp_tparams tparams
            pp_tparams tparams_hidden (P.pp_list pp_typ ", ") typs)
  | DefT typ -> F.fprintf fmt "typedef %a" pp_typ typ
  | NewT (id, typ) -> F.fprintf fmt "type %a (%a)" P.pp_id' id pp_typ typ
  | EnumT (id, fields) ->
      F.fprintf fmt "enum %a { %a }" P.pp_id' id
        (P.pp_list P.pp_member' ", ")
        fields
  | SEnumT (id, typ, fields) ->
      F.fprintf fmt "enum<%a> %a { %a }" pp_typ typ P.pp_id' id
        (P.pp_pairs P.pp_member' Value.pp " = " ", ")
        fields
  | ListT typ -> F.fprintf fmt "list<%a>" pp_typ typ
  | TupleT typs -> F.fprintf fmt "tuple<%a>" (P.pp_list pp_typ ", ") typs
  | StackT (typ, size) -> F.fprintf fmt "%a[%a]" pp_typ typ Bigint.pp size
  | StructT (id, fields) ->
      F.fprintf fmt "struct %a { %a }" P.pp_id' id
        (P.pp_pairs P.pp_member' pp_typ " " "; ")
        fields
  | HeaderT (id, fields) ->
      F.fprintf fmt "header %a { %a }" P.pp_id' id
        (P.pp_pairs P.pp_member' pp_typ " " "; ")
        fields
  | UnionT (id, fields) ->
      F.fprintf fmt "header_union %a { %a }" P.pp_id' id
        (P.pp_pairs P.pp_member' pp_typ " " "; ")
        fields
  | ExternT (id, fdenv) ->
      F.fprintf fmt "extern %a %a" P.pp_id' id (FIdMap.pp pp_funcdef) fdenv
  | ParserT params -> F.fprintf fmt "parser (%a)" pp_params params
  | ControlT params -> F.fprintf fmt "control (%a)" pp_params params
  | PackageT typs -> F.fprintf fmt "package { %a }" (P.pp_list pp_typ ", ") typs
  | TableT _ -> F.pp_print_string fmt "table"
  | AnyT -> F.pp_print_string fmt "any"
  | TableEnumT (id, _) -> F.fprintf fmt "enum_table %a" P.pp_id' id
  | TableStructT (id, fields) ->
      F.fprintf fmt "struct_table %a { %a }" P.pp_id' id
        (P.pp_pairs P.pp_member' pp_typ " " "; ")
        fields
  | SeqT typs -> F.fprintf fmt "seq<%a>" (P.pp_list pp_typ ", ") typs
  | SeqDefaultT typs ->
      F.fprintf fmt "seq<%a, ...>" (P.pp_list pp_typ ", ") typs
  | RecordT fields ->
      F.fprintf fmt "record { %a }"
        (P.pp_pairs P.pp_member' pp_typ " " "; ")
        fields
  | RecordDefaultT fields ->
      F.fprintf fmt "record { %a, ... }"
        (P.pp_pairs P.pp_member' pp_typ " " "; ")
        fields
  | DefaultT -> F.pp_print_string fmt "default"
  | InvalidT -> F.pp_print_string fmt "{#}"
  | SetT typ -> F.fprintf fmt "set<%a>" pp_typ typ
  | StateT -> F.pp_print_string fmt "state"

(* Type definitions *)

and pp_typdef fmt typdef =
  match typdef with
  | MonoD typ -> F.fprintf fmt "%a" pp_typ typ
  | PolyD (tparams, tparams_hidden, typ) ->
      F.fprintf fmt "%a<%a @@ %a>" pp_typ typ pp_tparams tparams pp_tparams
        tparams_hidden

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
  | MonoFD ft -> F.fprintf fmt "%a" pp_functyp ft
  | PolyFD (tparams, tparams_hidden, ft) ->
      F.fprintf fmt "%a<%a @@ %a>" pp_functyp ft pp_tparams tparams pp_tparams
        tparams_hidden

(* Constructor types *)

let pp_constyp fmt constyp =
  let cparams, typ = constyp in
  F.fprintf fmt "constructor (%a) -> %a" pp_cparams cparams pp_typ typ

(* Constructor definitions *)

let pp_consdef fmt consdef =
  let tparams, tparams_hidden, cparams, typ = consdef in
  F.fprintf fmt "constructor<%a @@ %a> (%a) -> %a" pp_tparams tparams pp_tparams
    tparams_hidden pp_cparams cparams pp_typ typ
