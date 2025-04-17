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
let string_of_atoms atoms = atoms |> List.map string_of_atom |> String.concat ""

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

let rec string_of_exp exp =
  match exp.it with
  | Il.Ast.BoolE b -> string_of_bool b
  | Il.Ast.NumE n -> string_of_num n
  | Il.Ast.TextE text -> "\"" ^ String.escaped text ^ "\""
  | Il.Ast.VarE varid -> string_of_varid varid
  | Il.Ast.UnE (unop, _, exp) -> string_of_unop unop ^ string_of_exp exp
  | Il.Ast.BinE (binop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | Il.Ast.CmpE (cmpop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | Il.Ast.UpCastE (typ, exp) ->
      "(" ^ string_of_exp exp ^ " as " ^ string_of_typ typ ^ ")"
  | Il.Ast.DownCastE (typ, exp) ->
      "(" ^ string_of_exp exp ^ " as " ^ string_of_typ typ ^ ")"
  | Il.Ast.SubE (exp, typ) ->
      "(" ^ string_of_exp exp ^ " has type " ^ string_of_typ typ ^ ")"
  | Il.Ast.MatchE (exp, pattern) ->
      "(" ^ string_of_exp exp ^ " matches pattern " ^ string_of_pattern pattern
      ^ ")"
  | Il.Ast.TupleE es -> "(" ^ string_of_exps ", " es ^ ")"
  | Il.Ast.CaseE notexp -> "(" ^ string_of_notexp notexp ^ ")"
  | Il.Ast.StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> string_of_atom atom ^ " " ^ string_of_exp exp)
             expfields)
      ^ "}"
  | Il.Ast.OptE (Some exp) -> "?(" ^ string_of_exp exp ^ ")"
  | Il.Ast.OptE None -> "?()"
  | Il.Ast.ListE exps -> "[" ^ string_of_exps ", " exps ^ "]"
  | Il.Ast.ConsE (exp_h, exp_t) ->
      string_of_exp exp_h ^ " :: " ^ string_of_exp exp_t
  | Il.Ast.CatE (exp_l, exp_r) ->
      string_of_exp exp_l ^ " ++ " ^ string_of_exp exp_r
  | Il.Ast.MemE (exp_e, exp_s) ->
      string_of_exp exp_e ^ " is in " ^ string_of_exp exp_s
  | Il.Ast.LenE exp -> "|" ^ string_of_exp exp ^ "|"
  | Il.Ast.DotE (exp_b, atom) -> string_of_exp exp_b ^ "." ^ string_of_atom atom
  | Il.Ast.IdxE (exp_b, exp_i) ->
      string_of_exp exp_b ^ "[" ^ string_of_exp exp_i ^ "]"
  | Il.Ast.SliceE (exp_b, exp_l, exp_h) ->
      string_of_exp exp_b ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | Il.Ast.UpdE (exp_b, path, exp_f) ->
      string_of_exp exp_b ^ "[" ^ string_of_path path ^ " = "
      ^ string_of_exp exp_f ^ "]"
  | Il.Ast.CallE (defid, targs, args) ->
      string_of_defid defid ^ string_of_targs targs ^ string_of_args args
  | Il.Ast.HoldE (relid, notexp) ->
      "(" ^ string_of_relid relid ^ ": " ^ string_of_notexp notexp ^ " holds"
      ^ ")"
  | Il.Ast.IterE (exp, iterexp) -> string_of_exp exp ^ string_of_iterexp iterexp

and string_of_exps sep exps = String.concat sep (List.map string_of_exp exps)

and string_of_notexp notexp =
  let mixop, exps = notexp in
  let atoms_h, mixop_t = (List.hd mixop, List.tl mixop) in
  string_of_atoms atoms_h
  :: List.map2
       (fun exp_t atoms_t -> string_of_exp exp_t ^ string_of_atoms atoms_t)
       exps mixop_t
  |> String.concat " "

and string_of_iterexp iterexp =
  let iter, _ = iterexp in
  string_of_iter iter

and string_of_iterexps iterexps =
  iterexps |> List.map string_of_iterexp |> String.concat ""

(* Patterns *)

and string_of_pattern pattern = Il.Print.string_of_pattern pattern

(* Paths *)

and string_of_path path =
  match path.it with
  | Il.Ast.RootP -> ""
  | Il.Ast.IdxP (path, exp) ->
      string_of_path path ^ "[" ^ string_of_exp exp ^ "]"
  | Il.Ast.SliceP (path, exp_l, exp_h) ->
      string_of_path path ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | Il.Ast.DotP ({ it = Il.Ast.RootP; _ }, atom) -> string_of_atom atom
  | Il.Ast.DotP (path, atom) -> string_of_path path ^ "." ^ string_of_atom atom

(* Parameters *)

and string_of_param param = Il.Print.string_of_param param
and string_of_params params = Il.Print.string_of_params params

(* Type parameters *)

and string_of_tparam tparam = Il.Print.string_of_tparam tparam
and string_of_tparams tparams = Il.Print.string_of_tparams tparams

(* Arguments *)

and string_of_arg arg =
  match arg.it with
  | Il.Ast.ExpA exp -> string_of_exp exp
  | Il.Ast.DefA defid -> string_of_defid defid

and string_of_args args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map string_of_arg args) ^ ")"

(* Type arguments *)

and string_of_targ targ = Il.Print.string_of_targ targ
and string_of_targs targs = Il.Print.string_of_targs targs

(* Path conditions *)

and string_of_pathcond pathcond =
  match pathcond with
  | ForallC (exp, iterexps) ->
      Format.asprintf "(forall %s)%s" (string_of_exp exp)
        (string_of_iterexps iterexps)
  | ExistsC (exp, iterexps) ->
      Format.asprintf "(exists %s)%s" (string_of_exp exp)
        (string_of_iterexps iterexps)
  | PlainC exp -> "(" ^ string_of_exp exp ^ ")"

and string_of_pathconds pathconds =
  List.map string_of_pathcond pathconds |> String.concat " /\\ "

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
      Format.asprintf "%sIf %s, then\n\n%s" order_leading
        (string_of_iterated string_of_exp exp_cond iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
  | IfI (exp_cond, iterexps, instrs_then, [ ({ it = IfI _; _ } as instr_else) ])
    ->
      Format.asprintf "%sIf %s, then\n\n%s\n\n%sElse %s" order_leading
        (string_of_iterated string_of_exp exp_cond iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
        order_following
        (string_of_instr ~inline:true ~level ~index instr_else)
  | IfI (exp_cond, iterexps, instrs_then, instrs_else) ->
      Format.asprintf "%sIf %s, then\n\n%s\n\n%sElse\n\n%s" order_leading
        (string_of_iterated string_of_exp exp_cond iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
        order_following
        (string_of_instrs ~level:(level + 1) instrs_else)
  | OtherwiseI instr ->
      Format.asprintf "%sOtherwise\n\n%s" order_leading
        (string_of_instr ~level:(level + 1) ~index:1 instr)
  | LetI (exp_l, exp_r, iterexps) ->
      Format.asprintf "%s%s" order_leading
        (string_of_iterated
           (fun (exp_l, exp_r) ->
             Format.asprintf "Let %s be %s" (string_of_exp exp_l)
               (string_of_exp exp_r))
           (exp_l, exp_r) iterexps)
  | ResultI [] -> Format.asprintf "%sThe relation holds" order_leading
  | ResultI exps ->
      Format.asprintf "%sResult in %s" order_leading (string_of_exps ", " exps)
  | ReturnI exp ->
      Format.asprintf "%sReturn %s" order_leading (string_of_exp exp)
  | PhantomI (pid, pathconds) ->
      Format.asprintf "%sPhantom#%d %s" order_leading pid
        (string_of_pathconds pathconds)

and string_of_instrs ?(level = 0) instrs =
  instrs
  |> List.mapi (fun idx instr -> string_of_instr ~level ~index:(idx + 1) instr)
  |> String.concat "\n\n"

(* Definitions *)

let rec string_of_def def =
  ";; " ^ string_of_region def.at ^ "\n"
  ^
  match def.it with
  | TypD (typid, tparams, deftyp) ->
      "syntax " ^ string_of_typid typid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | RelD (relid, (_mixop, _inputs), exps_input, instrs) ->
      "relation " ^ string_of_relid relid ^ ": "
      ^ string_of_exps ", " exps_input
      ^ "\n\n" ^ string_of_instrs instrs
  | DecD (defid, tparams, args_input, instrs) ->
      "def " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_args args_input ^ "\n\n" ^ string_of_instrs instrs

and string_of_defs defs = String.concat "\n\n" (List.map string_of_def defs)

(* Spec *)

let string_of_spec spec = string_of_defs spec
