open Domain.Dom
open Tdom
module L = Lang.Ast
module P = Lang.Pp
open Util.Pp

(* Pretty-printers *)

(* Type parameters *)

let rec pp_tparam fmt tparam = P.pp_tparam' fmt tparam

and pp_tparams fmt (tparams, tparams_hidden) =
  match (tparams, tparams_hidden) with
  | [], [] -> ()
  | tparams, [] -> F.fprintf fmt "<%a>" (pp_list pp_tparam ~sep:Comma) tparams
  | [], tparams_hidden ->
      F.fprintf fmt "<@@ %a>" (pp_list pp_tparam ~sep:Comma) tparams_hidden
  | tparams, tparams_hidden ->
      F.fprintf fmt "<%a @@ %a>"
        (pp_list pp_tparam ~sep:Comma)
        tparams
        (pp_list pp_tparam ~sep:Comma)
        tparams_hidden

(* Parameters *)

and pp_param ?(level = 0) fmt param =
  let id, dir, typ, value_default = param in
  match value_default with
  | Some value_default ->
      if dir = L.No then
        F.fprintf fmt "%a %a = %a" P.pp_id' id
          (pp_typ ~level:(level + 1))
          typ
          (Value.pp ~level:(level + 1))
          value_default
      else
        F.fprintf fmt "%a %a %a = %a" P.pp_dir' dir P.pp_id' id
          (pp_typ ~level:(level + 1))
          typ
          (Value.pp ~level:(level + 1))
          value_default
  | None ->
      if dir = L.No then
        F.fprintf fmt "%a %a" P.pp_id' id (pp_typ ~level:(level + 1)) typ
      else
        F.fprintf fmt "%a %a %a" P.pp_dir' dir P.pp_id' id
          (pp_typ ~level:(level + 1))
          typ

and pp_params ?(level = 0) fmt params =
  match params with
  | [] -> F.pp_print_string fmt "()"
  | [ param ] -> F.fprintf fmt "(%a)" (pp_param ~level) param
  | params ->
      F.fprintf fmt "(\n%a)"
        (pp_list ~level (pp_param ~level) ~sep:CommaNl)
        params

(* Constructor parameters *)

and pp_cparam ?(level = 0) fmt cparam = pp_param ~level fmt cparam
and pp_cparams ?(level = 0) fmt cparams = pp_params ~level fmt cparams

(* Types *)

and pp_typ ?(level = 0) fmt typ =
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
      F.fprintf fmt "(%a%a)<%a>"
        (pp_typ ~level:(level + 1))
        typ pp_tparams (tparams, tparams_hidden)
        (pp_list (pp_typ ~level:(level + 1)) ~sep:Comma)
        typs
  | DefT typ -> F.fprintf fmt "typedef %a" (pp_typ ~level:(level + 1)) typ
  | NewT (id, typ) ->
      F.fprintf fmt "type %a (%a)" P.pp_id' id (pp_typ ~level:(level + 1)) typ
  | EnumT (id, fields) ->
      F.fprintf fmt "enum %a { %a }" P.pp_id' id
        (pp_list P.pp_member' ~sep:Comma)
        fields
  | SEnumT (id, typ, fields) ->
      F.fprintf fmt "enum<%a> %a { %a }"
        (pp_typ ~level:(level + 1))
        typ P.pp_id' id
        (pp_pairs P.pp_member' Value.pp ~rel:Eq ~sep:Comma)
        fields
  | ListT typ -> F.fprintf fmt "list<%a>" (pp_typ ~level:(level + 1)) typ
  | TupleT typs ->
      F.fprintf fmt "tuple<%a>"
        (pp_list (pp_typ ~level:(level + 1)) ~sep:Comma)
        typs
  | StackT (typ, size) ->
      F.fprintf fmt "%a[%a]" (pp_typ ~level:(level + 1)) typ Bigint.pp size
  | StructT (id, fields) ->
      F.fprintf fmt "struct %a {\n%a\n%s}" P.pp_id' id
        (pp_pairs ~level:(level + 1) P.pp_member'
           (pp_typ ~level:(level + 1))
           ~rel:Space ~sep:SemicolonNl)
        fields (indent level)
  | HeaderT (id, fields) ->
      F.fprintf fmt "header %a {\n%a\n%s}" P.pp_id' id
        (pp_pairs ~level:(level + 1) P.pp_member'
           (pp_typ ~level:(level + 1))
           ~rel:Space ~sep:SemicolonNl)
        fields (indent level)
  | UnionT (id, fields) ->
      F.fprintf fmt "header_union %a {\n%a\n%s}" P.pp_id' id
        (pp_pairs ~level:(level + 1) P.pp_member'
           (pp_typ ~level:(level + 1))
           ~rel:Space ~sep:SemicolonNl)
        fields (indent level)
  | ExternT (id, fdenv) ->
      F.fprintf fmt "extern %a %a" P.pp_id' id
        (FIdMap.pp ~level:(level + 1) pp_funcdef)
        fdenv
  | ParserT params ->
      F.fprintf fmt "parser %a" (pp_params ~level:(level + 1)) params
  | ControlT params ->
      F.fprintf fmt "control %a" (pp_params ~level:(level + 1)) params
  | PackageT typs ->
      F.fprintf fmt "package {\n%a\n%s}"
        (pp_list ~level:(level + 1) (pp_typ ~level:(level + 1)) ~sep:Comma)
        typs (indent level)
  | TableT (id, _) -> F.fprintf fmt "table %a" P.pp_id' id
  | AnyT -> F.pp_print_string fmt "any"
  | TableEnumT (id, _) -> F.fprintf fmt "enum_table %a" P.pp_id' id
  | TableStructT (id, fields) ->
      F.fprintf fmt "struct_table %a {\n%a\n%s}" P.pp_id' id
        (pp_pairs ~level:(level + 1) P.pp_member'
           (pp_typ ~level:(level + 1))
           ~rel:Space ~sep:SemicolonNl)
        fields (indent level)
  | SeqT typs ->
      F.fprintf fmt "seq<%a>"
        (pp_list (pp_typ ~level:(level + 1)) ~sep:Comma)
        typs
  | SeqDefaultT typs ->
      F.fprintf fmt "seq<%a, ...>"
        (pp_list (pp_typ ~level:(level + 1)) ~sep:Comma)
        typs
  | RecordT fields ->
      F.fprintf fmt "record {\n%a\n%s}"
        (pp_pairs ~level:(level + 1) P.pp_member'
           (pp_typ ~level:(level + 1))
           ~rel:Space ~sep:SemicolonNl)
        fields (indent level)
  | RecordDefaultT fields ->
      F.fprintf fmt "record {\n%a...\n%s}"
        (pp_pairs ~trailing:true ~level:(level + 1) P.pp_member'
           (pp_typ ~level:(level + 1))
           ~rel:Space ~sep:SemicolonNl)
        fields (indent level)
  | DefaultT -> F.pp_print_string fmt "default"
  | InvalidT -> F.pp_print_string fmt "{#}"
  | SetT typ -> F.fprintf fmt "set<%a>" (pp_typ ~level:(level + 1)) typ
  | StateT -> F.pp_print_string fmt "state"

(* Type definitions *)

and pp_typdef ?(level = 0) fmt typdef =
  match typdef with
  | MonoD tdm -> pp_typdef_mono ~level fmt tdm
  | PolyD tdp -> pp_typdef_poly ~level fmt tdp

and pp_typdef_mono ?(level = 0) fmt tdm = pp_typ ~level fmt tdm

and pp_typdef_poly ?(level = 0) fmt tdp =
  let tparams, tparams_hidden, typ = tdp in
  F.fprintf fmt "%a%a" (pp_typ ~level) typ pp_tparams (tparams, tparams_hidden)

(* Member definitions *)

(* Function types *)

and pp_functyp ?(level = 0) fmt functyp =
  match functyp with
  | ActionT params ->
      F.fprintf fmt "action%a" (pp_params ~level:(level + 1)) params
  | ExternFunctionT (params, typ) ->
      F.fprintf fmt "extern_func%a -> %a"
        (pp_params ~level:(level + 1))
        params
        (pp_typ ~level:(level + 1))
        typ
  | FunctionT (params, typ) ->
      F.fprintf fmt "func%a -> %a"
        (pp_params ~level:(level + 1))
        params
        (pp_typ ~level:(level + 1))
        typ
  | BuiltinMethodT (params, typ) ->
      F.fprintf fmt "builtin_method%a -> %a"
        (pp_params ~level:(level + 1))
        params
        (pp_typ ~level:(level + 1))
        typ
  | ExternMethodT (params, typ) ->
      F.fprintf fmt "extern_method%a -> %a"
        (pp_params ~level:(level + 1))
        params
        (pp_typ ~level:(level + 1))
        typ
  | ExternAbstractMethodT (params, typ) ->
      F.fprintf fmt "extern_abstract_method%a -> %a"
        (pp_params ~level:(level + 1))
        params
        (pp_typ ~level:(level + 1))
        typ
  | ParserApplyMethodT params ->
      F.fprintf fmt "parser_apply%a" (pp_params ~level:(level + 1)) params
  | ControlApplyMethodT params ->
      F.fprintf fmt "control_apply%a" (pp_params ~level:(level + 1)) params
  | TableApplyMethodT _ -> F.fprintf fmt "table_apply"

(* Function definitions *)

and pp_funcdef ?(level = 0) fmt funcdef =
  match funcdef with
  | MonoFD ft -> F.fprintf fmt "%a" (pp_functyp ~level) ft
  | PolyFD (tparams, tparams_hidden, ft) ->
      F.fprintf fmt "%a%a" (pp_functyp ~level) ft pp_tparams
        (tparams, tparams_hidden)

(* Constructor types *)

let pp_constyp ?(level = 0) fmt constyp =
  let cparams, typ = constyp in
  F.fprintf fmt "constructor%a -> %a"
    (pp_cparams ~level:(level + 1))
    cparams
    (pp_typ ~level:(level + 1))
    typ

(* Constructor definitions *)

let pp_consdef ?(level = 0) fmt consdef =
  let tparams, tparams_hidden, constyp = consdef in
  F.fprintf fmt "(%a)%a"
    (pp_constyp ~level:(level + 1))
    constyp pp_tparams (tparams, tparams_hidden)
