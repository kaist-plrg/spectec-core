(** Shorthand pass: rewrites PL [LetI]/[IfI]/[CaseI] sequences into [CheckLetI]
    / [DestructI] / [OptionGetI] after annotation. *)

open Common.Source
module Pl = Lang.Pl

let rec eq_exp_var (a : Pl.exp) (b : Pl.exp) : bool =
  match (a.node.it, b.node.it) with
  | Pl.VarE id_a, Pl.VarE id_b -> id_a.it = id_b.it
  | Pl.IterE (a_inner, _), Pl.IterE (b_inner, _) -> eq_exp_var a_inner b_inner
  | _ -> false

let is_underscored (id : Pl.id) : bool =
  String.length id.it > 0 && id.it.[0] = '_'

let mk_instr (src : Pl.instr) (it : Pl.instr') : Pl.instr =
  { node = it $$ (src.node.at, src.node.note); hints = src.hints }

let is_scrut_alias (exp_scrut : Pl.exp) (exp_rhs : Pl.exp) : bool =
  match exp_rhs.node.it with
  | Pl.DownCastE (_, exp_scrut') -> eq_exp_var exp_scrut exp_scrut'
  | _ -> eq_exp_var exp_scrut exp_rhs

let strip_leading_rename (exp_scrut : Pl.exp) (block : Pl.instr list) :
    (Pl.exp * Pl.instr list) option =
  match block with
  | { node = { it = Pl.LetI (exp_target, exp_rhs, []); _ }; _ } :: rest
    when is_scrut_alias exp_scrut exp_rhs ->
      Some (exp_target, rest)
  | _ -> None

(* Single-arm form *)
let shorten_check_let (instr : Pl.instr) : Pl.instr option =
  let try_lift (exp_scrut : Pl.exp) (block : Pl.instr list) : Pl.instr option =
    strip_leading_rename exp_scrut block
    |> Option.map (fun (exp_target, rest) ->
           mk_instr instr (Pl.CheckLetI (exp_target, exp_scrut, rest)))
  in
  match instr.node.it with
  | Pl.IfI (exp_cond, [], block, _phantom) -> (
      match exp_cond.node.it with
      | Pl.SubE (exp_scrut, _) | Pl.MatchE (exp_scrut, _) ->
          try_lift exp_scrut block
      | _ -> None)
  | Pl.CaseI (exp_scrut, [ ((Pl.SubG _ | Pl.MatchG _), block) ], _phantom) ->
      try_lift exp_scrut block
  | _ -> None

(* Per-arm form for multi-arm CaseI *)
let shorten_case_let_guard (exp_scrut : Pl.exp) ((guard, block) : Pl.case) :
    Pl.case =
  match (strip_leading_rename exp_scrut block, guard) with
  | Some (exp_target, rest), Pl.SubG typ ->
      (Pl.CheckLetSubG (typ, exp_target), rest)
  | Some (exp_target, rest), Pl.MatchG patt ->
      (Pl.CheckLetMatchG (patt, exp_target), rest)
  | _ -> (guard, block)

let shorten_destruct (instr : Pl.instr) : Pl.instr option =
  match instr.node.it with
  | Pl.LetI (exp_l, exp_r, []) -> (
      match (exp_l.node.it, instr.hints.prose_fields) with
      | Pl.CaseE notexp, Some fields ->
          let exps_l = Lang.Il.Mixfix.args notexp in
          if List.length exps_l <> List.length fields then None
          else
            let destruct_fields =
              List.combine exps_l fields
              |> List.map (fun ((e : Pl.exp), name) ->
                     let visible =
                       match e.node.it with
                       | Pl.VarE id when is_underscored id -> false
                       | Pl.IterE ({ node = { it = Pl.VarE id; _ }; _ }, _)
                         when is_underscored id ->
                           false
                       | _ -> true
                     in
                     ((if visible then Some name else None), e))
            in
            if List.for_all (fun (n, _) -> n = None) destruct_fields then None
            else Some (mk_instr instr (Pl.DestructI (destruct_fields, exp_r)))
      | _ -> None)
  | _ -> None

let shorten_instr (instr : Pl.instr) : Pl.instr list =
  match shorten_check_let instr with
  | Some instr' -> [ instr' ]
  | None -> (
      match shorten_destruct instr with
      | Some instr' -> [ instr' ]
      | None -> [ instr ])

(* Sequence of two instructions:
     LetI (tmp = call); IfI (tmp matches Some, [LetI (target = tmp); rest])
   becomes
     OptionGetI (target, call); rest *)
let shorten_option_get (instrs : Pl.instr list) :
    (Pl.instr list * Pl.instr list) option =
  match instrs with
  | i1 :: i2 :: rest -> (
      match (i1.node.it, i2.node.it) with
      | ( Pl.LetI (exp_tmp, exp_call, []),
          Pl.IfI (exp_cond, [], inner_block, _phantom) ) -> (
          match (exp_cond.node.it, inner_block) with
          | Pl.MatchE (exp_scrut, OptP `Some), inner :: body_rest
            when eq_exp_var exp_tmp exp_scrut -> (
              match inner.node.it with
              | Pl.LetI (exp_target, exp_tmp', [])
                when eq_exp_var exp_tmp exp_tmp' ->
                  Some
                    ( mk_instr i1 (Pl.OptionGetI (exp_target, exp_call))
                      :: body_rest,
                      rest )
              | _ -> None)
          | _ -> None)
      | _ -> None)
  | _ -> None

(* Recursive traversal *)

let rec shorten_block (block : Pl.instr list) : Pl.instr list =
  let block = shorten_block_seq block in
  let block = List.concat_map shorten_instr block in
  List.map recurse_into_nested block

and recurse_into_nested (instr : Pl.instr) : Pl.instr =
  let it' : Pl.instr' =
    match instr.node.it with
    | Pl.IfI (cond, iterexps, block_then, phantom) ->
        Pl.IfI (cond, iterexps, shorten_block block_then, phantom)
    | Pl.RelAssertI { call; expect; iterexps; block; phantom } ->
        Pl.RelAssertI
          { call; expect; iterexps; block = shorten_block block; phantom }
    | Pl.CaseI (exp, cases, phantom) ->
        let cases' =
          cases
          |> List.map (shorten_case_let_guard exp)
          |> List.map (fun (guard, block) -> (guard, shorten_block block))
        in
        Pl.CaseI (exp, cases', phantom)
    | Pl.OtherwiseI inner -> Pl.OtherwiseI (recurse_into_nested inner)
    | Pl.TryI arms -> Pl.TryI (List.map shorten_block arms)
    | Pl.CheckLetI (e_l, e_r, block_inner) ->
        Pl.CheckLetI (e_l, e_r, shorten_block block_inner)
    | Pl.LetI _ | Pl.RelI _ | Pl.ResultI _ | Pl.ReturnI _ | Pl.DebugI _
    | Pl.DestructI _ | Pl.OptionGetI _ ->
        instr.node.it
  in
  { instr with node = it' $$ (instr.node.at, instr.node.note) }

and shorten_block_seq (instrs : Pl.instr list) : Pl.instr list =
  match instrs with
  | [] -> []
  | _ -> (
      match shorten_option_get instrs with
      | Some (shortened, rest) -> shortened @ shorten_block_seq rest
      | None -> List.hd instrs :: shorten_block_seq (List.tl instrs))

(* Entry points *)

let shorten_def (def : Pl.def) : Pl.def =
  let it' : Pl.def' =
    match def.node.it with
    | Pl.RelD (id, sig_, exps, block, elseblock_opt) ->
        Pl.RelD
          ( id,
            sig_,
            exps,
            shorten_block block,
            Option.map shorten_block elseblock_opt )
    | Pl.DecD (id, tparams, args, block, elseblock_opt) ->
        Pl.DecD
          ( id,
            tparams,
            args,
            shorten_block block,
            Option.map shorten_block elseblock_opt )
    | Pl.TypD _ | Pl.BuiltinDecD _ -> def.node.it
  in
  { def with node = it' $ def.node.at }

let shorten_spec (spec : Pl.spec) : Pl.spec = List.map shorten_def spec
