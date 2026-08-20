open Common.Domain
open Types

type t = IdSet.t

let empty = IdSet.empty
let ( + ) = IdSet.union
let unions (sets : t list) : t = List.fold_left ( + ) empty sets
let free_exp = Il.Free.free_exp
let free_exps = Il.Free.free_exps
let free_args = Il.Free.free_args

let free_iterexp ((_, vars) : iterexp) : t =
  vars |> List.map (fun ({ Il.varid; _ } : var) -> varid) |> IdSet.of_list

let free_iterexps (iterexps : iterexp list) : t =
  unions (List.map free_iterexp iterexps)

let free_relcall ({ notexp; _ } : relcall) : t =
  free_exps (Il.Mixfix.args notexp)

let free_guard (guard : guard) : t =
  match guard with
  | BoolG _ | SubG _ | MatchG _ -> empty
  | CmpG (_, _, exp) | MemG exp -> free_exp exp

let rec free_instr (instr : instr) : t =
  match instr.it with
  | RelI { call; iterexps; block } | RelAssertI { call; iterexps; block; _ } ->
      free_relcall call + free_iterexps iterexps + free_block block
  | IfI (exp_cond, iterexps, block, _) ->
      free_exp exp_cond + free_iterexps iterexps + free_block block
  | CaseI (exp, cases, _) ->
      free_exp exp
      + unions
          (List.map
             (fun (guard, block) -> free_guard guard + free_block block)
             cases)
  | OtherwiseI instr -> free_instr instr
  | LetI (exp_l, exp_r, iterexps, block) ->
      free_exp exp_l + free_exp exp_r + free_iterexps iterexps
      + free_block block
  | ResultI exps -> free_exps exps
  | ReturnI exp -> free_exp exp
  | DebugI (exp, instr) -> free_exp exp + free_instr instr

and free_block (block : block) : t = unions (List.map free_instr block)

let free_elseblock_opt (elseblock_opt : elseblock option) : t =
  elseblock_opt |> Option.map free_block |> Option.value ~default:empty

let free_def (def : def) : t =
  match def.it with
  | TypD _ | BuiltinDecD _ -> empty
  | RelD (_, mode, block, elseblock_opt) ->
      free_exps (Il.Mode.inputs mode)
      + free_block block
      + free_elseblock_opt elseblock_opt
  | DecD (_, _, args, block, elseblock_opt) ->
      free_args args + free_block block + free_elseblock_opt elseblock_opt
