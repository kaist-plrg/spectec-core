open Ast
open Util.Source

(* Numbers *)

let string_of_num num = Il.Print.string_of_num num

(* Texts *)

let string_of_text text = Il.Print.string_of_text text

(* Identifiers *)

let string_of_varid varid = Il.Print.string_of_varid varid
let string_of_typid typid = Il.Print.string_of_typid typid
let string_of_relid relid = Il.Print.string_of_relid relid
let string_of_ruleid ruleid = Il.Print.string_of_ruleid ruleid
let string_of_defid defid = Il.Print.string_of_defid defid

(* Atoms *)

let string_of_atom atom = Il.Print.string_of_atom atom
let string_of_atoms atoms = Il.Print.string_of_atoms atoms

(* Mixfix operators *)

let string_of_mixop mixop = Il.Print.string_of_mixop mixop

(* Iterators *)

let string_of_iter iter = Il.Print.string_of_iter iter

(* Variables *)

let string_of_var var = Il.Print.string_of_var var

(* Types *)

let string_of_typ typ = Il.Print.string_of_typ typ
let string_of_typs sep typs = Il.Print.string_of_typs sep typs
let string_of_nottyp nottyp = Il.Print.string_of_nottyp nottyp
let string_of_deftyp deftyp = Il.Print.string_of_deftyp deftyp
let string_of_typfield typfield = Il.Print.string_of_typfield typfield

let string_of_typfields sep typfields =
  Il.Print.string_of_typfields sep typfields

let string_of_typcase typcase = Il.Print.string_of_typcase typcase
let string_of_typcases sep typcases = Il.Print.string_of_typcases sep typcases

(* Values *)

let string_of_value ?(short = false) ?(level = 0) value =
  Il.Print.string_of_value ~short ~level value

(* Operators *)

let string_of_unop unop = Il.Print.string_of_unop unop
let string_of_binop binop = Il.Print.string_of_binop binop
let string_of_cmpop cmpop = Il.Print.string_of_cmpop cmpop

(* Expressions *)

let string_of_exp exp = Il.Print.string_of_exp exp
let string_of_exps sep exps = Il.Print.string_of_exps sep exps

let string_of_notexp ?(typ = None) notexp =
  Il.Print.string_of_notexp ~typ notexp

let string_of_iterexp iterexp = Il.Print.string_of_iterexp iterexp

(* Patterns *)

let string_of_pattern pattern = Il.Print.string_of_pattern pattern

(* Paths *)

let string_of_path path = Il.Print.string_of_path path

(* Parameters *)

let string_of_param param = Il.Print.string_of_param param
let string_of_params params = Il.Print.string_of_params params

(* Type parameters *)

let string_of_tparam tparam = Il.Print.string_of_tparam tparam
let string_of_tparams tparams = Il.Print.string_of_tparams tparams

(* Arguments *)

let string_of_arg arg = Il.Print.string_of_arg arg
let string_of_args args = Il.Print.string_of_args args

(* Type arguments *)

let string_of_targ targ = Il.Print.string_of_targ targ
let string_of_targs targs = Il.Print.string_of_targs targs

(* Instructions *)

let rec string_of_instr ?(level = 0) ?(index = 0) instr =
  let indent = String.make level ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  match instr.it with
  | RuleI (id_rel, notexp, []) ->
      Format.asprintf "%s%s: %s" order (string_of_relid id_rel)
        (string_of_notexp notexp)
  | RuleI (id_rel, notexp, iterexps) ->
      Format.asprintf "(%s%s: %s)%s" order (string_of_relid id_rel)
        (string_of_notexp notexp)
        (String.concat "" (List.map string_of_iterexp iterexps))
  | IfI (exp_cond, [], instrs_then, []) ->
      Format.asprintf "%sIf %s, then\n%s" order (string_of_exp exp_cond)
        (string_of_instrs ~level:(level + 1) instrs_then)
  | IfI (exp_cond, iterexps, instrs_then, []) ->
      Format.asprintf "%sIf (%s)%s, then\n%s" order (string_of_exp exp_cond)
        (String.concat "" (List.map string_of_iterexp iterexps))
        (string_of_instrs ~level:(level + 1) instrs_then)
  | IfI (exp_cond, [], instrs_then, instrs_else) ->
      Format.asprintf "%sIf %s, then\n%s%sElse\n%s" order
        (string_of_exp exp_cond)
        (string_of_instrs ~level:(level + 1) instrs_then)
        indent
        (string_of_instrs ~level:(level + 1) instrs_else)
  | IfI (exp_cond, iterexps, instrs_then, instrs_else) ->
      Format.asprintf "%sIf (%s)%s, then\n%s%sElse\n%s" order
        (string_of_exp exp_cond)
        (String.concat "" (List.map string_of_iterexp iterexps))
        (string_of_instrs ~level:(level + 1) instrs_then)
        indent
        (string_of_instrs ~level:(level + 1) instrs_else)
  | ElseI instr ->
      Format.asprintf "%sOtherwise\n%s" order
        (string_of_instr ~level:(level + 1) instr)
  | LetI (exp_l, exp_r, []) ->
      Format.asprintf "%sLet %s = %s" order (string_of_exp exp_l)
        (string_of_exp exp_r)
  | LetI (exp_l, exp_r, iterexps) ->
      Format.asprintf "%s(Let %s = %s)%s" order (string_of_exp exp_l)
        (string_of_exp exp_r)
        (String.concat "" (List.map string_of_iterexp iterexps))
  | RetRelI exps ->
      Format.asprintf "%sResult in %s" order (string_of_exps ", " exps)
  | RetDecI exp -> Format.asprintf "%sReturn %s" order (string_of_exp exp)

and string_of_instrs ?(level = 0) instrs =
  instrs
  |> List.mapi (fun idx instr -> string_of_instr ~level ~index:(idx + 1) instr)
  |> String.concat "\n"

(* Definitions *)

let rec string_of_def def =
  ";; " ^ string_of_region def.at ^ "\n"
  ^
  match def.it with
  | TypD (typid, tparams, deftyp) ->
      "syntax " ^ string_of_typid typid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | RelD (relid, exps_input, instrs) ->
      "relation " ^ string_of_relid relid ^ ": "
      ^ string_of_exps ", " exps_input
      ^ "\n"
      ^ string_of_instrs ~level:1 instrs
  | DecD (defid, tparams, args_input, instrs) ->
      "def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_args args_input ^ "\n"
      ^ string_of_instrs ~level:1 instrs

and string_of_defs defs = String.concat "\n\n" (List.map string_of_def defs)

(* Spec *)

let string_of_spec spec = string_of_defs spec
