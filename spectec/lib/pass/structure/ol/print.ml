open Common.Source
open Lang.Il.Print
open Ast

(* Cases *)

let rec string_of_case ?(level = 0) ?(index = 0) case =
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
  | MemG exp -> "(% in " ^ string_of_exp exp ^ ")"

(* Instructions *)

and string_of_instr ?(level = 0) ?(index = 0) instr =
  let indent = String.make (level * 2) ' ' in
  let order = Format.asprintf "%s%d. " indent index in
  match instr.it with
  | RelI { call = { relid; notexp }; iterexps; block } ->
      Format.asprintf "%s(%s: %s)%s" order (string_of_relid relid)
        (string_of_notexp notexp)
        (string_of_iterexps iterexps)
      ^ "\n\n"
      ^ string_of_instrs ~level:(level + 1) block
  | RelAssertI
      { call = { relid; notexp }; expect; iterexps; block = instrs_then } ->
      Format.asprintf "%sIf (%s: %s %s)%s, then\n\n%s" order
        (string_of_relid relid) (string_of_notexp notexp)
        (if expect then "holds" else "does not hold")
        (string_of_iterexps iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
  | IfI (exp_cond, iterexps, instrs_then) ->
      Format.asprintf "%sIf (%s)%s, then\n\n%s" order (string_of_exp exp_cond)
        (string_of_iterexps iterexps)
        (string_of_instrs ~level:(level + 1) instrs_then)
  | CaseI (exp, cases, _) ->
      Format.asprintf "%sCase analysis on %s\n\n%s" order (string_of_exp exp)
        (string_of_cases ~level:(level + 1) cases)
  | OtherwiseI instr ->
      Format.asprintf "%sOtherwise\n\n%s" order
        (string_of_instr ~level:(level + 1) ~index:1 instr)
  | LetI (exp_l, exp_r, iterexps, block) ->
      Format.asprintf "%s(Let %s be %s)%s" order (string_of_exp exp_l)
        (string_of_exp exp_r)
        (string_of_iterexps iterexps)
      ^ "\n\n"
      ^ string_of_instrs ~level:(level + 1) block
  | ResultI [] -> Format.asprintf "%sThe relation holds" order
  | ResultI exps ->
      Format.asprintf "%sResult in %s" order (string_of_exps ", " exps)
  | ReturnI exp -> Format.asprintf "%sReturn %s" order (string_of_exp exp)
  | DebugI (exp, instr_body) ->
      Format.asprintf "%sDebug: %s" order (string_of_exp exp)
      ^ "\n\n"
      ^ string_of_instr ~level ~index:(index + 1) instr_body

and string_of_instrs ?(level = 0) instrs =
  instrs
  |> List.mapi (fun idx instr -> string_of_instr ~level ~index:(idx + 1) instr)
  |> String.concat "\n\n"
