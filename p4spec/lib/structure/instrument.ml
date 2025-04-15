open Sl.Ast
open Util.Source

let negate_exp (exp : exp) : exp =
  Il.Ast.UnE (`NotOp, `BoolT, exp) $$ (exp.at, exp.note)

let negate_pathcond (pathcond : pathcond) : pathcond =
  match pathcond with
  | ForallC (exp_cond, iterexps) -> ExistsC (negate_exp exp_cond, iterexps)
  | ExistsC (exp_cond, iterexps) -> ForallC (negate_exp exp_cond, iterexps)
  | PlainC exp_cond -> PlainC (negate_exp exp_cond)

let rec insert_phantom' ?(pathconds : pathcond list = []) (instrs : instr list)
    : instr list =
  match instrs with
  | [] -> []
  | ({ it = IfI (exp_cond, iterexps, instrs_then, []); _ } as instr_h)
    :: instrs_t ->
      let pathcond =
        if iterexps = [] then PlainC exp_cond else ForallC (exp_cond, iterexps)
      in
      let instrs_then =
        let pathconds = pathconds @ [ pathcond ] in
        insert_phantom' ~pathconds instrs_then
      in
      let instr_else =
        let pathconds = pathconds @ [ negate_pathcond pathcond ] in
        PhantomI pathconds $ no_region
      in
      let instr_h =
        IfI (exp_cond, iterexps, instrs_then, [ instr_else ]) $ instr_h.at
      in
      let instrs_t = insert_phantom' ~pathconds instrs_t in
      instr_h :: instrs_t
  | ({ it = IfI (exp_cond, iterexps, instrs_then, instrs_else); _ } as instr_h)
    :: instrs_t ->
      let pathcond =
        if iterexps = [] then PlainC exp_cond else ForallC (exp_cond, iterexps)
      in
      let instrs_then =
        let pathconds = pathconds @ [ pathcond ] in
        insert_phantom' ~pathconds instrs_then
      in
      let instrs_else =
        let pathconds = pathconds @ [ negate_pathcond pathcond ] in
        insert_phantom' ~pathconds instrs_else
      in
      let instr_h =
        IfI (exp_cond, iterexps, instrs_then, instrs_else) $ instr_h.at
      in
      let instrs_t = insert_phantom' ~pathconds instrs_t in
      instr_h :: instrs_t
  | instr_h :: instrs_t ->
      let instrs_t = insert_phantom' ~pathconds instrs_t in
      instr_h :: instrs_t

let insert_phantom (instrs : instr list) : instr list =
  if
    List.exists
      (fun instr -> match instr.it with OtherwiseI _ -> true | _ -> false)
      instrs
  then instrs
  else insert_phantom' instrs
