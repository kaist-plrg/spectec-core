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
let string_of_iterexps iterexps = Il.Print.string_of_iterexps iterexps

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

let string_of_iterated (string_of : 'a -> string) (iterated : 'a)
    (iterexps : iterexp list) : string =
  match iterexps with
  | [] -> string_of iterated
  | _ ->
      Format.asprintf "(%s)%s" (string_of iterated)
        (string_of_iterexps iterexps)

let rec string_of_instr ?(inline = false) ?(level = 0) ?(index = 0) instr =
  let indent = String.make (level * 2) ' ' in
  let order_leading =
    if inline then "" else Format.asprintf "%s%d. " indent index
  in
  let order_following = Format.asprintf "%s%d. " indent index in
  match instr.it with
  | RuleI (id_rel, notexp, iterexps) ->
      Format.asprintf "%s%s" order_leading
        (string_of_iterated
           (fun (id_rel, notexp) ->
             Format.asprintf "%s: %s" (string_of_relid id_rel)
               (string_of_notexp notexp))
           (id_rel, notexp) iterexps)
  | IfI (exp_cond, iterexps, instrs_then, []) ->
      Format.asprintf "%sIf %s, then\n%s" order_leading
        (string_of_iterated string_of_exp exp_cond iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
  | IfI (exp_cond, iterexps, instrs_then, [ ({ it = IfI _; _ } as instr_else) ])
    ->
      Format.asprintf "%sIf %s, then\n%s\n%sElse %s" order_leading
        (string_of_iterated string_of_exp exp_cond iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
        order_following
        (string_of_instr ~inline:true ~level ~index instr_else)
  | IfI (exp_cond, iterexps, instrs_then, instrs_else) ->
      Format.asprintf "%sIf %s, then\n%s\n%sElse\n%s" order_leading
        (string_of_iterated string_of_exp exp_cond iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
        order_following
        (string_of_instrs ~level:(level + 1) instrs_else)
  | OtherwiseI instr ->
      Format.asprintf "%sOtherwise\n%s" order_leading
        (string_of_instr ~level:(level + 1) ~index:1 instr)
  | LetI (exp_l, exp_r, iterexps) ->
      Format.asprintf "%s%s" order_leading
        (string_of_iterated
           (fun (exp_l, exp_r) ->
             Format.asprintf "Let %s = %s" (string_of_exp exp_l)
               (string_of_exp exp_r))
           (exp_l, exp_r) iterexps)
  | RetRelI exps ->
      Format.asprintf "%sResult in %s" order_leading (string_of_exps ", " exps)
  | RetDecI exp ->
      Format.asprintf "%sReturn %s" order_leading (string_of_exp exp)

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
      ^ "\n" ^ string_of_instrs instrs
  | DecD (defid, tparams, args_input, instrs) ->
      "def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_args args_input ^ "\n" ^ string_of_instrs instrs

and string_of_defs defs = String.concat "\n\n" (List.map string_of_def defs)

(* Spec *)

let string_of_spec spec = string_of_defs spec
