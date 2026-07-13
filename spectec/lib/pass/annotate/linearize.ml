open Common.Source
module Sl = Lang.Sl

let rec linearize_instr (instr : Sl.instr) : Ll.block =
  let at = instr.at in
  match instr.it with
  | Sl.RelI { call; iterexps; block } ->
      let block_ll = linearize_block block in
      let instr_ll = Ll.RelI { call; iterexps } $ at in
      instr_ll :: block_ll
  | Sl.RelAssertI { call; expect; iterexps; block = block_then; phantom } ->
      let block_then_ll = linearize_block block_then in
      [
        Ll.RelAssertI { call; expect; iterexps; block = block_then_ll; phantom }
        $ at;
      ]
  | Sl.IfI (exp_cond, iterexps, block_then, phantom_opt) ->
      let block_then_ll = linearize_block block_then in
      [ Ll.IfI (exp_cond, iterexps, block_then_ll, phantom_opt) $ at ]
  | Sl.CaseI (exp, cases, phantom_opt) ->
      let cases_ll =
        List.map
          (fun (guard, block) ->
            let block_ll = linearize_block block in
            (linearize_guard guard, block_ll))
          cases
      in
      [ Ll.CaseI (exp, cases_ll, phantom_opt) $ at ]
  | Sl.OtherwiseI instr_inner ->
      let block_ll = linearize_instr instr_inner in
      [ Ll.OtherwiseI block_ll $ at ]
  | Sl.LetI (exp_l, exp_r, iterexps, block) ->
      let block_ll = linearize_block block in
      let instr_ll = Ll.LetI (exp_l, exp_r, iterexps) $ at in
      instr_ll :: block_ll
  | Sl.ResultI exps -> [ Ll.ResultI exps $ at ]
  | Sl.ReturnI exp -> [ Ll.ReturnI exp $ at ]
  | Sl.DebugI (exp, body) ->
      let instr_debug = Ll.DebugI exp $ at in
      instr_debug :: linearize_instr body

and linearize_block (block : Sl.block) : Ll.block =
  block |> List.concat_map linearize_instr |> wrap_try_arms

and linearize_guard (g : Sl.guard) : Ll.guard =
  match g with
  | Sl.BoolG b -> Ll.BoolG b
  | Sl.CmpG (op, optyp, e) -> Ll.CmpG (op, optyp, e)
  | Sl.SubG typ -> Ll.SubG typ
  | Sl.MatchG pat -> Ll.MatchG pat
  | Sl.MemG e -> Ll.MemG e

and is_branching (instr : Ll.instr) : bool =
  match instr.it with
  | Ll.IfI _ | Ll.RelAssertI _ | Ll.CaseI _ -> true
  | _ -> false

and split_leading_branches (instrs : Ll.block) : Ll.instr list * Ll.block =
  match instrs with
  | instr :: rest when is_branching instr ->
      let branches, remainder = split_leading_branches rest in
      (instr :: branches, remainder)
  | _ -> ([], instrs)

and wrap_try_arms (instrs : Ll.block) : Ll.block =
  match split_leading_branches instrs with
  | [], [] -> []
  | [], head :: rest -> head :: wrap_try_arms rest
  | [ single ], remainder -> single :: wrap_try_arms remainder
  | branches, remainder ->
      let arms = List.map (fun i -> [ i ]) branches in
      let wrapped = Ll.TryI arms $ no_region in
      wrapped :: wrap_try_arms remainder

let linearize_elseblock (elseblock : Sl.elseblock) : Ll.block =
  let block_ll = linearize_block elseblock in
  [ Ll.OtherwiseI block_ll $ no_region ]

let linearize_elseblock_opt (elseblock_opt : Sl.elseblock option) : Ll.block =
  match elseblock_opt with
  | Some elseblock -> linearize_elseblock elseblock
  | None -> []

let linearize_def (def : Sl.def) : Ll.def =
  let at = def.at in
  match def.it with
  | Sl.TypD (id, tparams, deftyp) -> Ll.TypD (id, tparams, deftyp) $ at
  | Sl.RelD (id, mode, block, elseblock_opt) ->
      let mixop = Lang.Il.Mixfix.to_mixop mode in
      let inputs =
        Lang.Il.Mixfix.args mode
        |> List.mapi (fun i arg ->
               match arg with Lang.Il.Mode.In _ -> Some i | Out _ -> None)
        |> List.filter_map Fun.id
      in
      let exps = Lang.Il.Mode.inputs mode in
      let block_ll = linearize_block block in
      let elseblock_ll = linearize_elseblock_opt elseblock_opt in
      let elseblock_opt_ll =
        if elseblock_ll = [] then None else Some elseblock_ll
      in
      Ll.RelD (id, (mixop, inputs), exps, block_ll, elseblock_opt_ll) $ at
  | Sl.BuiltinDecD (id, tparams, args) ->
      Ll.BuiltinDecD (id, tparams, args) $ at
  | Sl.DecD (id, tparams, args, block, elseblock_opt) ->
      let block_ll = linearize_block block in
      let elseblock_ll = linearize_elseblock_opt elseblock_opt in
      let elseblock_opt_ll =
        if elseblock_ll = [] then None else Some elseblock_ll
      in
      Ll.DecD (id, tparams, args, block_ll, elseblock_opt_ll) $ at

let linearize_spec (spec : Sl.spec) : Ll.spec = List.map linearize_def spec
