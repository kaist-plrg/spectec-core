open Xl
open Ast
open Util.Source

(* Numbers *)

let string_of_num num = Num.string_of_num num

(* Texts *)

let string_of_text text = text

(* Identifiers *)

let string_of_varid varid = varid.it
let string_of_typid typid = typid.it
let string_of_relid relid = relid.it
let string_of_ruleid ruleid = if ruleid.it = "" then "" else "/" ^ ruleid.it
let string_of_defid defid = "$" ^ defid.it

(* Atoms *)

let string_of_atom atom = Atom.string_of_atom atom.it

(* Iterators *)

let string_of_iter iter = match iter with Opt -> "?" | List -> "*"

(* Types *)

let rec string_of_plaintyp plaintyp =
  match plaintyp.it with
  | BoolT -> "bool"
  | NumT numtyp -> Num.string_of_typ numtyp
  | TextT -> "text"
  | VarT (typid, targs) -> string_of_typid typid ^ string_of_targs targs
  | ParenT plaintyp -> "(" ^ string_of_plaintyp plaintyp ^ ")"
  | TupleT plaintyps -> "(" ^ string_of_plaintyps ", " plaintyps ^ ")"
  | IterT (plaintyp, iter) -> string_of_plaintyp plaintyp ^ string_of_iter iter

and string_of_plaintyps sep plaintyps =
  String.concat sep (List.map string_of_plaintyp plaintyps)

and string_of_nottyp nottyp =
  match nottyp.it with
  | PlainT plaintyp -> string_of_plaintyp plaintyp
  | AtomT atom -> string_of_atom atom
  | SeqT nottyps -> "{" ^ string_of_nottyps " " nottyps ^ "}"
  | InfixT (nottyp_l, atom, nottyp_r) ->
      string_of_nottyp nottyp_l ^ " " ^ string_of_atom atom ^ " "
      ^ string_of_nottyp nottyp_r
  | BrackT (atom_l, nottyp, atom_r) ->
      "`" ^ string_of_atom atom_l ^ string_of_nottyp nottyp
      ^ string_of_atom atom_r

and string_of_nottyps sep nottyps =
  String.concat sep (List.map string_of_nottyp nottyps)

and string_of_deftyp deftyp =
  match deftyp.it with
  | NotationT nottyp -> string_of_nottyp nottyp
  | StructT typfields -> "{" ^ string_of_typfields ", " typfields ^ "}"
  | VariantT typcases -> "| " ^ string_of_typcases " | " typcases

and string_of_typfield typfield =
  let atom, plaintyp, _hints = typfield in
  string_of_atom atom ^ " " ^ string_of_plaintyp plaintyp

and string_of_typfields sep typfields =
  String.concat sep (List.map string_of_typfield typfields)

and string_of_typcase typcase =
  let nottyp, _hints = typcase in
  string_of_nottyp nottyp

and string_of_typcases sep typcases =
  String.concat sep (List.map string_of_typcase typcases)

(* Operators *)

and string_of_unop = function
  | #Bool.unop as op -> Bool.string_of_unop op
  | #Num.unop as op -> Num.string_of_unop op

and string_of_binop = function
  | #Bool.binop as op -> Bool.string_of_binop op
  | #Num.binop as op -> Num.string_of_binop op

and string_of_cmpop = function
  | #Bool.cmpop as op -> Bool.string_of_cmpop op
  | #Num.cmpop as op -> Num.string_of_cmpop op

(* Expressions *)

and string_of_exp exp =
  match exp.it with
  | BoolE b -> string_of_bool b
  | NumE (`DecOp, `Nat n) -> Z.to_string n
  | NumE (`HexOp, `Nat n) -> "0x" ^ Z.format "%X" n
  | NumE (_, n) -> string_of_num n
  | TextE text -> "\"" ^ String.escaped text ^ "\""
  | VarE (id, targs) -> string_of_varid id ^ string_of_targs targs
  | UnE (unop, exp) -> string_of_unop unop ^ string_of_exp exp
  | BinE (exp_l, binop, exp_r) ->
      string_of_exp exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ string_of_exp exp_r
  | CmpE (exp_l, cmpop, exp_r) ->
      string_of_exp exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ string_of_exp exp_r
  | EpsE -> "eps"
  | ListE exps -> "[" ^ string_of_exps ", " exps ^ "]"
  | IdxE (exp_b, exp_i) -> string_of_exp exp_b ^ "[" ^ string_of_exp exp_i ^ "]"
  | SliceE (exp_b, exp_l, exp_h) ->
      string_of_exp exp_b ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | UpdE (exp_b, path, exp_f) ->
      string_of_exp exp_b ^ "[" ^ string_of_path path ^ " = "
      ^ string_of_exp exp_f ^ "]"
  | StrE fields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> string_of_atom atom ^ " " ^ string_of_exp exp)
             fields)
      ^ "}"
  | DotE (exp, atom) -> string_of_exp exp ^ "." ^ string_of_atom atom
  | CatE (exp_l, exp_r) -> string_of_exp exp_l ^ " ++ " ^ string_of_exp exp_r
  | MemE (exp_e, exp_s) -> string_of_exp exp_e ^ " <- " ^ string_of_exp exp_s
  | LenE exp -> "|" ^ string_of_exp exp ^ "|"
  | ParenE exp -> "(" ^ string_of_exp exp ^ ")"
  | TupleE exps -> "(" ^ string_of_exps ", " exps ^ ")"
  | CallE (id, targs, args) ->
      string_of_defid id ^ string_of_targs targs ^ string_of_args args
  | IterE (exp, iter) -> string_of_exp exp ^ string_of_iter iter
  | TypE (exp, plaintyp) ->
      string_of_exp exp ^ " : " ^ string_of_plaintyp plaintyp
  | ArithE exp -> "$(" ^ string_of_exp exp ^ ")"
  | AtomE atom -> string_of_atom atom
  | SeqE exps -> string_of_exps " " exps
  | InfixE (exp_l, atom, exp_r) ->
      string_of_exp exp_l ^ " " ^ string_of_atom atom ^ " "
      ^ string_of_exp exp_r
  | BrackE (atom_l, exp, atom_r) ->
      "`" ^ string_of_atom atom_l ^ string_of_exp exp ^ string_of_atom atom_r
  | HoleE (`Num i) -> "%" ^ string_of_int i
  | HoleE `Next -> "%"
  | HoleE `Rest -> "%%"
  | HoleE `None -> "!%"
  | FuseE (exp_l, exp_r) -> string_of_exp exp_l ^ "#" ^ string_of_exp exp_r
  | UnparenE exp -> "##" ^ string_of_exp exp
  | LatexE s -> "latex(" ^ String.escaped s ^ ")"

and string_of_exps sep es = String.concat sep (List.map string_of_exp es)

(* Paths *)

and string_of_path path =
  match path.it with
  | RootP -> ""
  | IdxP (path, exp) -> string_of_path path ^ "[" ^ string_of_exp exp ^ "]"
  | SliceP (path, exp_l, exp_h) ->
      string_of_path path ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | DotP ({ it = RootP; _ }, atom) -> string_of_atom atom
  | DotP (path, atom) -> string_of_path path ^ "." ^ string_of_atom atom

(* Parameters *)

and string_of_param param =
  match param.it with
  | ExpP plaintyp -> string_of_plaintyp plaintyp
  | DefP (defid, tparams, params, plaintyp) ->
      string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : "
      ^ string_of_plaintyp plaintyp

and string_of_params params =
  match params with
  | [] -> ""
  | params -> "(" ^ String.concat ", " (List.map string_of_param params) ^ ")"

(* Type parameters *)

and string_of_tparam tparam = tparam.it

and string_of_tparams tparams =
  match tparams with
  | [] -> ""
  | tparams ->
      "<" ^ String.concat ", " (List.map string_of_tparam tparams) ^ ">"

(* Arguments *)

and string_of_arg arg =
  match arg.it with
  | ExpA exp -> string_of_exp exp
  | DefA defid -> string_of_defid defid

and string_of_args args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map string_of_arg args) ^ ")"

(* Type arguments *)

and string_of_targ targ = string_of_plaintyp targ

and string_of_targs targs =
  match targs with
  | [] -> ""
  | targs -> "<" ^ String.concat ", " (List.map string_of_targ targs) ^ ">"

(* Premises *)

and string_of_prem prem =
  match prem.it with
  | VarPr (id, plaintyp) ->
      string_of_varid id ^ " : " ^ string_of_plaintyp plaintyp
  | RulePr (id, exp) -> string_of_relid id ^ ": " ^ string_of_exp exp
  | IfPr exp -> "if " ^ string_of_exp exp
  | ElsePr -> "otherwise"
  | LetPr (exp_l, exp_r) ->
      "let " ^ string_of_exp exp_l ^ " = " ^ string_of_exp exp_r
  | IfLetPr (exp_l, exp_r) ->
      "iflet " ^ string_of_exp exp_l ^ " = " ^ string_of_exp exp_r
  | IterPr (({ it = IterPr _; _ } as prem), iter) ->
      string_of_prem prem ^ string_of_iter iter
  | IterPr (prem, iter) -> "(" ^ string_of_prem prem ^ ")" ^ string_of_iter iter

and string_of_prems prems =
  String.concat "" (List.map (fun prem -> "\n -- " ^ string_of_prem prem) prems)

(* Definitions *)

let string_of_def def =
  match def.it with
  | SynD (typid, tparams) ->
      "syntax " ^ string_of_typid typid ^ string_of_tparams tparams
  | TypD (typid, tparams, deftyp, _hints) ->
      "syntax " ^ string_of_typid typid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | VarD (varid, plaintyp, _hints) ->
      "var " ^ string_of_varid varid ^ " : " ^ string_of_plaintyp plaintyp
  | RelD (relid, nottyp, _hints) ->
      "relation " ^ string_of_relid relid ^ ": " ^ string_of_nottyp nottyp
  | RuleD (relid, ruleid, exp, prems) ->
      "rule " ^ string_of_relid relid ^ string_of_ruleid ruleid ^ ":\n  "
      ^ string_of_exp exp ^ string_of_prems prems
  | DecD (defid, tparams, params, plaintyp, _hints) ->
      "dec " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : "
      ^ string_of_plaintyp plaintyp
  | DefD (defid, targs, args, exp, prems) ->
      "def " ^ string_of_defid defid ^ string_of_targs targs
      ^ string_of_args args ^ " = " ^ string_of_exp exp ^ string_of_prems prems
  | SepD -> "\n\n"

(* Spec *)

let string_of_spec spec =
  String.concat "" (List.map (fun def -> string_of_def def ^ "\n") spec)
