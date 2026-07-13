open Common.Source
open Types

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

let string_of_vid vid = "@" ^ string_of_int vid

let string_of_value ?(short = false) ?(level = 0) value =
  Il.Print.string_of_value ~short ~level value

(* Operators *)

let string_of_unop unop = Il.Print.string_of_unop unop
let string_of_binop binop = Il.Print.string_of_binop binop
let string_of_cmpop cmpop = Il.Print.string_of_cmpop cmpop

(* Expressions *)

let rec string_of_exp exp =
  match exp.it with
  | Il.BoolE b -> string_of_bool b
  | Il.NumE n -> string_of_num n
  | Il.TextE text -> "\"" ^ String.escaped text ^ "\""
  | Il.VarE varid -> string_of_varid varid
  | Il.UnE (unop, _, exp) -> string_of_unop unop ^ string_of_exp exp
  | Il.BinE (binop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_binop binop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | Il.CmpE (cmpop, _, exp_l, exp_r) ->
      "(" ^ string_of_exp exp_l ^ " " ^ string_of_cmpop cmpop ^ " "
      ^ string_of_exp exp_r ^ ")"
  | Il.UpCastE (typ, exp) ->
      "(" ^ string_of_exp exp ^ " as " ^ string_of_typ typ ^ ")"
  | Il.DownCastE (typ, exp) ->
      "(" ^ string_of_exp exp ^ " as " ^ string_of_typ typ ^ ")"
  | Il.SubE (exp, typ) ->
      "(" ^ string_of_exp exp ^ " has type " ^ string_of_typ typ ^ ")"
  | Il.MatchE (exp, pattern) ->
      "(" ^ string_of_exp exp ^ " matches pattern " ^ string_of_pattern pattern
      ^ ")"
  | Il.TupleE es -> "(" ^ string_of_exps ", " es ^ ")"
  | Il.CaseE notexp -> "(" ^ string_of_notexp notexp ^ ")"
  | Il.StrE expfields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (atom, exp) -> string_of_atom atom ^ " " ^ string_of_exp exp)
             expfields)
      ^ "}"
  | Il.OptE (Some exp) -> "?(" ^ string_of_exp exp ^ ")"
  | Il.OptE None -> "?()"
  | Il.ListE exps -> "[" ^ string_of_exps ", " exps ^ "]"
  | Il.ConsE (exp_h, exp_t) ->
      string_of_exp exp_h ^ " :: " ^ string_of_exp exp_t
  | Il.CatE (exp_l, exp_r) -> string_of_exp exp_l ^ " ++ " ^ string_of_exp exp_r
  | Il.MemE (exp_e, exp_s) ->
      string_of_exp exp_e ^ " is in " ^ string_of_exp exp_s
  | Il.LenE exp -> "|" ^ string_of_exp exp ^ "|"
  | Il.DotE (exp_b, atom) -> string_of_exp exp_b ^ "." ^ string_of_atom atom
  | Il.IdxE (exp_b, exp_i) ->
      string_of_exp exp_b ^ "[" ^ string_of_exp exp_i ^ "]"
  | Il.SliceE (exp_b, exp_l, exp_h) ->
      string_of_exp exp_b ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | Il.UpdE (exp_b, path, exp_f) ->
      string_of_exp exp_b ^ "[" ^ string_of_path path ^ " = "
      ^ string_of_exp exp_f ^ "]"
  | Il.CallE (defid, targs, args) ->
      string_of_defid defid ^ string_of_targs targs ^ string_of_args args
  | Il.IterE (exp, iterexp) -> string_of_exp exp ^ string_of_iterexp iterexp

and string_of_exps sep exps = String.concat sep (List.map string_of_exp exps)

and string_of_notexp notexp =
  Il.Mixfix.render ~pad_brackets:true ~string_of_atom
    ~string_of_arg:string_of_exp notexp

and string_of_iterexp (iter, _) = Il.Print.string_of_iter iter

and string_of_iterexps iterexps =
  iterexps |> List.map string_of_iterexp |> String.concat ""

(* Patterns *)

and string_of_pattern pattern = Il.Print.string_of_pattern pattern

(* Paths *)

and string_of_path path =
  match path.it with
  | Il.RootP -> ""
  | Il.IdxP (path, exp) -> string_of_path path ^ "[" ^ string_of_exp exp ^ "]"
  | Il.SliceP (path, exp_l, exp_h) ->
      string_of_path path ^ "[" ^ string_of_exp exp_l ^ " : "
      ^ string_of_exp exp_h ^ "]"
  | Il.DotP ({ it = Il.RootP; _ }, atom) -> string_of_atom atom
  | Il.DotP (path, atom) -> string_of_path path ^ "." ^ string_of_atom atom

(* Parameters *)

and string_of_param param = Il.Print.string_of_param param
and string_of_params params = Il.Print.string_of_params params

(* Type parameters *)

and string_of_tparam tparam = Il.Print.string_of_tparam tparam
and string_of_tparams tparams = Il.Print.string_of_tparams tparams

(* Arguments *)

and string_of_arg arg =
  match arg.it with
  | Il.ExpA exp -> string_of_exp exp
  | Il.DefA defid -> string_of_defid defid

and string_of_args args =
  match args with
  | [] -> ""
  | args -> "(" ^ String.concat ", " (List.map string_of_arg args) ^ ")"

(* Type arguments *)

and string_of_targ targ = Il.Print.string_of_targ targ
and string_of_targs targs = Il.Print.string_of_targs targs

(* Path conditions *)

and string_of_pid pid = Format.asprintf "Phantom#%d" pid

and string_of_phantom phantom =
  let pid, _ = phantom in
  string_of_pid pid

and string_of_pathcond pathcond =
  match pathcond with
  | ForallC (pathcond, iterexps) ->
      Format.asprintf "(forall %s)%s"
        (string_of_pathcond pathcond)
        (string_of_iterexps iterexps)
  | ExistsC (pathcond, iterexps) ->
      Format.asprintf "(exists %s)%s"
        (string_of_pathcond pathcond)
        (string_of_iterexps iterexps)
  | PlainC exp -> "(" ^ string_of_exp exp ^ ")"
  | RelAssertC { call = { relid; notexp }; expect } ->
      Format.asprintf "(%s: %s %s)" (string_of_relid relid)
        (string_of_notexp notexp)
        (if expect then "holds" else "does not hold")

and string_of_pathconds pathconds =
  List.map string_of_pathcond pathconds |> String.concat " /\\ "

(* Case analysis *)

and string_of_case ?(level = 0) ?(index = 0) case =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  let guard, instrs = case in
  Format.asprintf "%sCase %s\n\n%s" order (string_of_guard guard)
    (string_of_instrs ~level:(level + 1) instrs)

and string_of_cases ?(level = 0) cases =
  cases
  |> List.mapi (fun idx case -> string_of_case ~level ~index:(idx + 1) case)
  |> String.concat "\n\n"

and string_of_guard guard =
  match guard with
  | BoolG b -> string_of_bool b
  | CmpG (cmpop, _, exp) ->
      "(% " ^ string_of_cmpop cmpop ^ " " ^ string_of_exp exp ^ ")"
  | SubG typ -> "(% has type " ^ string_of_typ typ ^ ")"
  | MatchG patten -> "(% matches pattern " ^ string_of_pattern patten ^ ")"
  | MemG exp -> "(% is in " ^ string_of_exp exp ^ ")"

(* Instructions *)

and string_of_instr ?(level = 0) ?(index = 0) instr =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  match instr.it with
  | RelI { call = { relid; notexp }; iterexps; block } ->
      Format.asprintf "%s(%s: %s)%s\n\n%s" order (string_of_relid relid)
        (string_of_notexp notexp)
        (string_of_iterexps iterexps)
        (fst (string_of_instrs_from ~level ~index:(index + 1) block))
  | RelAssertI
      {
        call = { relid; notexp };
        expect;
        iterexps;
        block = instrs_then;
        phantom = None;
      } ->
      Format.asprintf "%sIf (%s: %s %s)%s, then\n\n%s" order
        (string_of_relid relid) (string_of_notexp notexp)
        (if expect then "holds" else "does not hold")
        (string_of_iterexps iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
  | RelAssertI
      {
        call = { relid; notexp };
        expect;
        iterexps;
        block = instrs_then;
        phantom = Some phantom;
      } ->
      Format.asprintf "%sIf (%s: %s %s)%s, then\n\n%s\n\n%sElse %s" order
        (string_of_relid relid) (string_of_notexp notexp)
        (if expect then "holds" else "does not hold")
        (string_of_iterexps iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
        order
        (string_of_phantom phantom)
  | IfI (exp_cond, iterexps, instrs_then, None) ->
      Format.asprintf "%sIf (%s)%s, then\n\n%s" order (string_of_exp exp_cond)
        (string_of_iterexps iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
  | IfI (exp_cond, iterexps, instrs_then, Some phantom) ->
      Format.asprintf "%sIf (%s)%s, then\n\n%s\n\n%sElse %s" order
        (string_of_exp exp_cond)
        (string_of_iterexps iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
        order
        (string_of_phantom phantom)
  | CaseI (exp, cases, None) ->
      Format.asprintf "%sCase analysis on %s\n\n%s" order (string_of_exp exp)
        (string_of_cases ~level:(level + 1) cases)
  | CaseI (exp, cases, Some phantom) ->
      Format.asprintf "%sCase analysis on %s\n\n%s\n\n%sElse %s" order
        (string_of_exp exp)
        (string_of_cases ~level:(level + 1) cases)
        order
        (string_of_phantom phantom)
  | OtherwiseI instr ->
      Format.asprintf "%sOtherwise\n\n%s" order
        (string_of_instr ~level:(level + 1) ~index:1 instr)
  | LetI (exp_l, exp_r, iterexps, block) ->
      Format.asprintf "%s(Let %s be %s)%s\n\n%s" order (string_of_exp exp_l)
        (string_of_exp exp_r)
        (string_of_iterexps iterexps)
        (fst (string_of_instrs_from ~level ~index:(index + 1) block))
  | ResultI [] -> Format.asprintf "%sThe relation holds" order
  | ResultI exps ->
      Format.asprintf "%sResult in %s" order (string_of_exps ", " exps)
  | ReturnI exp -> Format.asprintf "%sReturn %s" order (string_of_exp exp)
  | DebugI (exp, instr_body) ->
      let body, _next =
        string_of_instr_with_next ~level ~index:(index + 1) instr_body
      in
      Format.asprintf "%sDebug: %s\n\n%s" order (string_of_exp exp) body

and string_of_instr_with_next ?(level = 0) ~(index : int) instr =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  match instr.it with
  | LetI (exp_l, exp_r, iterexps, block) ->
      let block, next = string_of_instrs_from ~level ~index:(index + 1) block in
      ( Format.asprintf "%s(Let %s be %s)%s\n\n%s" order (string_of_exp exp_l)
          (string_of_exp exp_r)
          (string_of_iterexps iterexps)
          block,
        next )
  | RelI { call = { relid; notexp }; iterexps; block } ->
      let block, next = string_of_instrs_from ~level ~index:(index + 1) block in
      ( Format.asprintf "%s(%s: %s)%s\n\n%s" order (string_of_relid relid)
          (string_of_notexp notexp)
          (string_of_iterexps iterexps)
          block,
        next )
  | DebugI (exp, instr_body) ->
      let body, next =
        string_of_instr_with_next ~level ~index:(index + 1) instr_body
      in
      (Format.asprintf "%sDebug: %s\n\n%s" order (string_of_exp exp) body, next)
  | _ -> (string_of_instr ~level ~index instr, index + 1)

and string_of_instrs_from ?(level = 0) ~(index : int) instrs =
  List.fold_left
    (fun (strings, index) instr ->
      let string, index = string_of_instr_with_next ~level ~index instr in
      (strings @ [ string ], index))
    ([], index) instrs
  |> fun (strings, index) -> (String.concat "\n\n" strings, index)

and string_of_instrs ?(level = 0) instrs =
  fst (string_of_instrs_from ~level ~index:1 instrs)

and string_of_block ?(level = 0) ?(index = 0) block =
  fst (string_of_instrs_from ~level ~index:(index + 1) block)

and string_of_elseblock ?(level = 0) ?(index = 0) elseblock =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent (index + 1) in
  Format.asprintf "%sOtherwise\n\n%s" order
    (string_of_instrs ~level:(level + 1) elseblock)

and string_of_elseblock_opt ?(level = 0) ?(index = 0) elseblock_opt =
  match elseblock_opt with
  | None -> ""
  | Some elseblock -> "\n\n" ^ string_of_elseblock ~level ~index elseblock

(* Definitions *)

let rec string_of_def def =
  ";; " ^ string_of_region def.at ^ "\n"
  ^
  match def.it with
  | TypD (typid, tparams, deftyp) ->
      "syntax " ^ string_of_typid typid ^ string_of_tparams tparams ^ " = "
      ^ string_of_deftyp deftyp
  | RelD (relid, mode, block, elseblock_opt) ->
      "relation " ^ string_of_relid relid ^ ": "
      ^ Il.Mode.render_inputs ~sep:", " ~string_of_arg:string_of_exp mode
      ^ "\n\n" ^ string_of_block block
      ^ string_of_elseblock_opt ~index:(List.length block) elseblock_opt
  | BuiltinDecD (defid, tparams, args_input) ->
      "builtin dec " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_args args_input
  | DecD (defid, tparams, args_input, block, elseblock_opt) ->
      "dec " ^ string_of_defid defid ^ string_of_tparams tparams
      ^ string_of_args args_input ^ "\n\n" ^ string_of_block block
      ^ string_of_elseblock_opt ~index:(List.length block) elseblock_opt

and string_of_defs defs = String.concat "\n\n" (List.map string_of_def defs)

(* Spec *)

let string_of_spec spec = string_of_defs spec
