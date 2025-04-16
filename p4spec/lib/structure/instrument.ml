open Sl.Ast
open Util.Source

(* Insert phantom instructions at dangling else branches,
   with the path condition necessary to reach the else branch

   Note that this does not take fall-through into account,
   so the path condition is not precise

   Fall-through may happen due to the heuristic-driven syntactic optimization of SL,

   (i) Good case

   -- if i >= 0   and   -- if i < 0
   -- if j >= 0         -- if j >= 0

   are nicely merged into

   if i >= 0 then
     if j >= 0 then ...
     else Phantom: i >= 0 && j < 0
   else
     if j >= 0 then ...
     else Phantom: i < 0 && j < 0

   (ii) Bad case

   -- if j >= 0   and  -- if i < 0
   -- if i >= 0        -- if j >= 0

   are merged into

   if j >= 0 then
     if i >= 0 then ...
     else Phantom: j >= 0 && i < 0
   else Phantom: j < 0

   ... if i = -1, j = 3 is given as input, it falls through

   if i < 0 then
      if j >= 0 then ...
      else Phantom: i < 0 && j < 0
   else Phantom: i >= 0 *)

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
