open Domain.Dom
open Tdom
module P = Lang.Pp
open Util.Pp

(* Pretty-printers *)

(* Type parameters *)

let rec pp_tparam fmt tparam = P.pp_tparam' fmt tparam
and pp_tparams fmt tparams = pp_list pp_tparam ", " fmt tparams

(* Parameters *)

and pp_param fmt param =
  let id, dir, typ, value_default = param in
  match value_default with
  | Some value_default ->
      F.fprintf fmt "%a %a %a = %a" P.pp_dir' dir P.pp_id' id (pp_typ ~level:0)
        typ (Value.pp ~level:0) value_default
  | None ->
      F.fprintf fmt "%a %a %a" P.pp_dir' dir P.pp_id' id (pp_typ ~level:0) typ

and pp_params fmt params = pp_list pp_param ", " fmt params

(* Constructor parameters *)

and pp_cparam fmt cparam = pp_param fmt cparam
and pp_cparams fmt cparams = pp_list pp_cparam ", " fmt cparams

(* Types *)

and pp_typ ?(level = 0) fmt typ =
  level |> ignore;
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
  | SpecT (tdp, typs) ->
      let tparams, tparams_hidden, typ = tdp in
      F.fprintf fmt "(%a<%a @@ %a>)<%a>"
        (pp_typ ~level:(level + 1))
        typ pp_tparams tparams pp_tparams tparams_hidden
        (pp_list (pp_typ ~level:(level + 1)) ", ")
        typs
  | DefT typ -> F.fprintf fmt "typedef %a" (pp_typ ~level:(level + 1)) typ
  | NewT (id, typ) ->
      F.fprintf fmt "type %a (%a)" P.pp_id' id (pp_typ ~level:(level + 1)) typ
  | EnumT (id, fields) ->
      F.fprintf fmt "enum %a { %a }" P.pp_id' id
        (pp_list P.pp_member' ", ")
        fields
  | SEnumT (id, typ, fields) ->
      F.fprintf fmt "enum<%a> %a { %a }"
        (pp_typ ~level:(level + 1))
        typ P.pp_id' id
        (pp_pairs P.pp_member' Value.pp " = " ", ")
        fields
  | ListT typ -> F.fprintf fmt "list<%a>" (pp_typ ~level:(level + 1)) typ
  | TupleT typs ->
      F.fprintf fmt "tuple<%a>" (pp_list (pp_typ ~level:(level + 1)) ", ") typs
  | StackT (typ, size) ->
      F.fprintf fmt "%a[%a]" (pp_typ ~level:(level + 1)) typ Bigint.pp size
  | StructT (id, fields) ->
      F.fprintf fmt "struct %a { %a }" P.pp_id' id
        (pp_pairs P.pp_member' (pp_typ ~level:(level + 1)) " " "; ")
        fields
  | HeaderT (id, fields) ->
      F.fprintf fmt "header %a { %a }" P.pp_id' id
        (pp_pairs P.pp_member' (pp_typ ~level:(level + 1)) " " "; ")
        fields
  | UnionT (id, fields) ->
      F.fprintf fmt "header_union %a { %a }" P.pp_id' id
        (pp_pairs P.pp_member' (pp_typ ~level:(level + 1)) " " "; ")
        fields
  | ExternT (id, fdenv) ->
      F.fprintf fmt "extern %a %a" P.pp_id' id (FIdMap.pp pp_funcdef) fdenv
  | ParserT params -> F.fprintf fmt "parser (%a)" pp_params params
  | ControlT params -> F.fprintf fmt "control (%a)" pp_params params
  | PackageT typs ->
      F.fprintf fmt "package { %a }"
        (pp_list (pp_typ ~level:(level + 1)) ", ")
        typs
  | TableT _ -> F.pp_print_string fmt "table"
  | AnyT -> F.pp_print_string fmt "any"
  | TableEnumT (id, _) -> F.fprintf fmt "enum_table %a" P.pp_id' id
  | TableStructT (id, fields) ->
      F.fprintf fmt "struct_table %a { %a }" P.pp_id' id
        (pp_pairs P.pp_member' (pp_typ ~level:(level + 1)) " " "; ")
        fields
  | SeqT typs ->
      F.fprintf fmt "seq<%a>" (pp_list (pp_typ ~level:(level + 1)) ", ") typs
  | SeqDefaultT typs ->
      F.fprintf fmt "seq<%a, ...>"
        (pp_list (pp_typ ~level:(level + 1)) ", ")
        typs
  | RecordT fields ->
      F.fprintf fmt "record { %a }"
        (pp_pairs P.pp_member' (pp_typ ~level:(level + 1)) " " "; ")
        fields
  | RecordDefaultT fields ->
      F.fprintf fmt "record { %a, ... }"
        (pp_pairs P.pp_member' (pp_typ ~level:(level + 1)) " " "; ")
        fields
  | DefaultT -> F.pp_print_string fmt "default"
  | InvalidT -> F.pp_print_string fmt "{#}"
  | SetT typ -> F.fprintf fmt "set<%a>" (pp_typ ~level:(level + 1)) typ
  | StateT -> F.pp_print_string fmt "state"

(* Type definitions *)

and pp_typdef ?(level = 0) fmt typdef =
  level |> ignore;
  match typdef with
  | MonoD tdm -> pp_typdef_mono fmt tdm
  | PolyD tdp -> pp_typdef_poly fmt tdp

and pp_typdef_mono fmt tdm = pp_typ fmt tdm

and pp_typdef_poly fmt tdp =
  let tparams, tparams_hidden, typ = tdp in
  F.fprintf fmt "%a<%a @@ %a>" (pp_typ ~level:0) typ pp_tparams tparams
    pp_tparams tparams_hidden

(* Member definitions *)

(* Function types *)

and pp_functyp fmt functyp =
  match functyp with
  | ActionT params -> F.fprintf fmt "action(%a)" pp_params params
  | ExternFunctionT (params, typ) ->
      F.fprintf fmt "extern_func(%a) -> %a" pp_params params (pp_typ ~level:0)
        typ
  | FunctionT (params, typ) ->
      F.fprintf fmt "func(%a) -> %a" pp_params params (pp_typ ~level:0) typ
  | ExternMethodT (params, typ) ->
      F.fprintf fmt "extern_method(%a) -> %a" pp_params params (pp_typ ~level:0)
        typ
  | ExternAbstractMethodT (params, typ) ->
      F.fprintf fmt "extern_abstract_method(%a) -> %a" pp_params params
        (pp_typ ~level:0) typ
  | ParserApplyMethodT params ->
      F.fprintf fmt "parser_apply(%a)" pp_params params
  | ControlApplyMethodT params ->
      F.fprintf fmt "control_apply(%a)" pp_params params
  | BuiltinMethodT (params, typ) ->
      F.fprintf fmt "builtin_method(%a) -> %a" pp_params params
        (pp_typ ~level:0) typ
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
  F.fprintf fmt "constructor (%a) -> %a" pp_cparams cparams (pp_typ ~level:0)
    typ

(* Constructor definitions *)

let pp_consdef fmt consdef =
  let tparams, tparams_hidden, cparams, typ = consdef in
  F.fprintf fmt "constructor<%a @@ %a> (%a) -> %a" pp_tparams tparams pp_tparams
    tparams_hidden pp_cparams cparams (pp_typ ~level:0) typ
