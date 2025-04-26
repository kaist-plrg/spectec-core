open Xl
open Ast
open Util.Print
open Util.Source

(* Numbers *)

let string_of_num = Num.string_of_num

(* Texts *)

let string_of_text text = text

(* Identifiers *)

let string_of_varid varid = varid.it
let string_of_typid typid = typid.it
let string_of_relid relid = relid.it
let string_of_ruleid ruleid = ruleid.it
let string_of_defid defid = "$" ^ defid.it

(* Atoms *)

let string_of_atom atom = Atom.string_of_atom atom.it

let string_of_atoms atoms =
  match atoms with
  | [] -> ""
  | _ ->
      Format.asprintf "`%s`"
        (atoms |> List.map string_of_atom |> String.concat "")

(* Mixfix operators *)

let string_of_mixop mixop = Mixop.string_of_mixop mixop

(* Iterators *)

let string_of_iter iter = match iter with Opt -> "?" | List -> "*"

(* Variables *)

let string_of_var (id, _typ, iters) =
  string_of_varid id ^ String.concat "" (List.map string_of_iter iters)

(* Types *)

let rec string_of_typ typ =
  match typ.it with
  | BoolT -> "bool"
  | NumT numtyp -> Num.string_of_typ numtyp
  | TextT -> "text"
  | VarT (typid, targs) -> string_of_typid typid ^ string_of_targs targs
  | TupleT typs -> "(" ^ string_of_typs ", " typs ^ ")"
  | IterT (typ, iter) -> string_of_typ typ ^ string_of_iter iter
  | FuncT -> "func"

and string_of_typs sep typs = String.concat sep (List.map string_of_typ typs)

and string_of_nottyp nottyp =
  let mixop, typs = nottyp.it in
  string_of_mixop mixop ^ "(" ^ string_of_typs ", " typs ^ ")"

and string_of_deftyp deftyp =
  match deftyp.it with
  | PlainT typ -> string_of_typ typ
  | StructT typfields -> "{" ^ string_of_typfields ", " typfields ^ "}"
  | VariantT typcases -> "\n   | " ^ string_of_typcases "\n   | " typcases

and string_of_typfield typfield =
  let atom, typ = typfield in
  string_of_nottyp (([ [ atom ]; [] ], [ typ ]) $ no_region)

and string_of_typfields sep typfields =
  String.concat sep (List.map string_of_typfield typfields)

and string_of_typcase typcase = string_of_nottyp typcase

and string_of_typcases sep typcases =
  String.concat sep (List.map string_of_typcase typcases)

(* Values *)

and string_of_value ?(short = false) ?(level = 0) value =
  match value with
  | BoolV b -> string_of_bool b
  | NumV n -> Num.string_of_num n
  | TextV s -> "\"" ^ s ^ "\""
  | StructV [] -> "{}"
  | StructV valuefields when short ->
      Format.asprintf "{ .../%d }" (List.length valuefields)
  | StructV valuefields ->
      Format.asprintf "{ %s }"
        (String.concat ";\n"
           (List.mapi
              (fun idx (atom, value) ->
                let indent = if idx = 0 then "" else indent (level + 1) in
                Format.asprintf "%s%s %s" indent (string_of_atom atom)
                  (string_of_value ~short ~level:(level + 2) value))
              valuefields))
  | CaseV (mixop, _) when short -> string_of_mixop mixop
  | CaseV (mixop, values) ->
      let atoms_h, mixop_t = (List.hd mixop, List.tl mixop) in
      Format.asprintf "(%s%s)" (string_of_atoms atoms_h)
        (String.concat ""
           (List.map2
              (fun value atoms ->
                string_of_value ~short ~level:(level + 1) value
                ^ string_of_atoms atoms)
              values mixop_t))
  | TupleV values ->
      Format.asprintf "(%s)"
        (String.concat ", "
           (List.map (string_of_value ~short ~level:(level + 1)) values))
  | OptV (Some value) ->
      Format.asprintf "Some(%s)"
        (string_of_value ~short ~level:(level + 1) value)
  | OptV None -> "None"
  | ListV [] -> "[]"
  | ListV values when short -> Format.asprintf "[ .../%d ]" (List.length values)
  | ListV values ->
      Format.asprintf "[ %s ]"
        (String.concat ",\n"
           (List.mapi
              (fun idx value ->
                let indent = if idx = 0 then "" else indent (level + 1) in
                indent ^ string_of_value ~short ~level:(level + 2) value)
              values))
  | FuncV id -> string_of_defid id

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
  | NumE n -> string_of_num n
  | TextE text -> "\"" ^ String.escaped text ^ "\""
  | VarE varid -> string_of_varid varid
  | UnE (unop, _, exp) -> string_of_unop unop ^ string_of_exp exp
  | BinE (binop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | CmpE (cmpop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | UpCastE (typ, exp) -> string_of_exp exp ^ " as " ^ string_of_typ typ
  | DownCastE (typ, exp) -> string_of_exp exp ^ " as " ^ string_of_typ typ
  | SubE (exp, typ) -> string_of_exp exp ^ " <: " ^ string_of_typ typ
  | MatchE (exp, pattern) ->
      string_of_exp exp ^ " matches " ^ string_of_pattern pattern
  | TupleE es -> "(" ^ string_of_exps ", " es ^ ")"
  | CaseE notexp -> string_of_notexp ~typ:(Some exp.note) notexp
  | StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> string_of_atom atom ^ " " ^ string_of_exp exp)
             expfields)
      ^ "}"
  | OptE exp_opt -> "?(" ^ string_of_exps "" (Option.to_list exp_opt) ^ ")"
  | ListE exps -> "[" ^ string_of_exps ", " exps ^ "]"
  | ConsE (exp_h, exp_t) -> string_of_exp exp_h ^ " :: " ^ string_of_exp exp_t
  | CatE (exp_l, exp_r) -> string_of_exp exp_l ^ " ++ " ^ string_of_exp exp_r
  | MemE (exp_e, exp_s) -> string_of_exp exp_e ^ " <- " ^ string_of_exp exp_s
  | LenE exp -> "|" ^ string_of_exp exp ^ "|"
  | DotE (exp_b, atom) -> string_of_exp exp_b ^ "." ^ string_of_atom atom
  | IdxE (exp_b, exp_i) -> string_of_exp exp_b ^ "[" ^ string_of_exp exp_i ^ "]"
  | SliceE (exp_b, exp_l, exp_h) ->
      string_of_exp exp_b ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | UpdE (exp_b, path, exp_f) ->
      string_of_exp exp_b ^ "[" ^ string_of_path path ^ " = "
      ^ string_of_exp exp_f ^ "]"
  | CallE (defid, targs, args) ->
      string_of_defid defid ^ string_of_targs targs ^ string_of_args args
  | HoldE (relid, notexp) ->
      string_of_relid relid ^ ": " ^ string_of_notexp notexp ^ " holds"
  | IterE (exp, iterexp) -> string_of_exp exp ^ string_of_iterexp iterexp

and string_of_exps sep exps = String.concat sep (List.map string_of_exp exps)

and string_of_notexp ?(typ = None) notexp =
  let mixop, exps = notexp in
  match typ with
  | Some typ ->
      string_of_mixop mixop ^ "_"
      ^ string_of_typ (typ $ no_region)
      ^ "(" ^ string_of_exps ", " exps ^ ")"
  | None -> string_of_mixop mixop ^ "(" ^ string_of_exps ", " exps ^ ")"

and string_of_iterexp iterexp =
  let iter, vars = iterexp in
  string_of_iter iter ^ "{"
  ^ String.concat ", "
      (List.map
         (fun var ->
           let id, typ, iters = var in
           string_of_var var ^ " <- " ^ string_of_var (id, typ, iters @ [ iter ]))
         vars)
  ^ "}"

and string_of_iterexps iterexps =
  iterexps |> List.map string_of_iterexp |> String.concat ""

(* Patterns *)

and string_of_pattern pattern =
  match pattern with
  | CaseP mixop -> string_of_mixop mixop
  | ListP `Cons -> "_ :: _"
  | ListP (`Fixed len) -> Format.asprintf "[ _/%d ]" len
  | ListP `Nil -> "[]"
  | OptP `Some -> "(_)"
  | OptP `None -> "()"

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
  | ExpP typ -> string_of_typ typ
  | DefP (defid, tparams, params, typ) ->
      string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ

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

and string_of_targ targ = string_of_typ targ

and string_of_targs targs =
  match targs with
  | [] -> ""
  | targs -> "<" ^ String.concat ", " (List.map string_of_targ targs) ^ ">"

(* Rules *)

and string_of_rule rule =
  let ruleid, notexp, prems = rule.it in
  ";; " ^ string_of_region rule.at ^ "\n   rule " ^ string_of_ruleid ruleid
  ^ ": " ^ string_of_notexp notexp ^ string_of_prems prems

and string_of_rules rules =
  String.concat ""
    (List.map (fun rule -> "\n\n   " ^ string_of_rule rule) rules)

(* Clause *)

and string_of_clause idx clause =
  let args, exp, prems = clause.it in
  ";; " ^ string_of_region clause.at ^ "\n   clause " ^ string_of_int idx
  ^ string_of_args args ^ " = " ^ string_of_exp exp ^ string_of_prems prems

and string_of_clauses clauses =
  String.concat ""
    (List.mapi
       (fun idx clause -> "\n\n   " ^ string_of_clause idx clause)
       clauses)

(* Premises *)

and string_of_prem prem =
  match prem.it with
  | RulePr (id, notexp) -> string_of_relid id ^ ": " ^ string_of_notexp notexp
  | IfPr exp -> "if " ^ string_of_exp exp
  | ElsePr -> "otherwise"
  | LetPr (exp_l, exp_r) ->
      "let " ^ string_of_exp exp_l ^ " = " ^ string_of_exp exp_r
  | IterPr (({ it = IterPr _; _ } as prem), iterexp) ->
      string_of_prem prem ^ string_of_iterexp iterexp
  | IterPr (prem, iterexp) ->
      "(" ^ string_of_prem prem ^ ")" ^ string_of_iterexp iterexp

and string_of_prems prems =
  String.concat ""
    (List.map (fun prem -> "\n      -- " ^ string_of_prem prem) prems)

(* Definitions *)

let rec string_of_def def =
  ";; " ^ string_of_region def.at ^ "\n"
  ^
  match def.it with
  | TypD (typid, tparams, deftyp) ->
      "syntax " ^ string_of_typid typid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | RelD (relid, nottyp, _, rules) ->
      "relation " ^ string_of_relid relid ^ ": " ^ string_of_nottyp nottyp
      ^ string_of_rules rules
  | DecD (defid, tparams, params, typ, clauses) ->
      "def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_params params ^ " : " ^ string_of_typ typ ^ " ="
      ^ string_of_clauses clauses

and string_of_defs defs = String.concat "\n\n" (List.map string_of_def defs)

(* Spec *)

let string_of_spec spec = string_of_defs spec
