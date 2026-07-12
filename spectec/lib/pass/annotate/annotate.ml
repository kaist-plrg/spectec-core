open Common.Source
module Sl = Lang.Sl
module Il = Lang.Il
module Linearize = Linearize
module Shorthand = Shorthand

let wrap_instr (instr' : Pl.instr' phrase) (hints : Pl.Annot.hints) : Pl.instr =
  { node = instr'; hints }

let wrap_def (def' : Pl.def' phrase) (hints : Pl.Annot.hints) : Pl.def =
  { node = def'; hints }

let wrap_exp (inner : (Pl.exp', Pl.typ') note_phrase) (hints : Pl.Annot.hints) :
    Pl.exp =
  { node = inner; hints }

let bare_instr instr' = wrap_instr instr' Pl.Annot.empty
let bare_def def' = wrap_def def' Pl.Annot.empty
let bare_exp inner = wrap_exp inner Pl.Annot.empty

let lookup_prose_out_rel (ctx : Ctx.t) (id_str : string) : Hints.Alter.t option
    =
  match Ctx.find_prose_out_rel ctx id_str with
  | None -> None
  | Some alter -> (
      match Ctx.find_rel_inputs ctx id_str with
      | Some inputs -> Some (Hints.Alter.realign alter inputs)
      | None -> Some alter)

let hints_of_call_exp (ctx : Ctx.t) (id : Il.id) : Pl.Annot.hints =
  let id_str = id.it in
  {
    Pl.Annot.empty with
    prose_in = Ctx.find_prose_in_func ctx id_str;
    prose_true = Ctx.find_prose_true_func ctx id_str;
    prose_false = Ctx.find_prose_false_func ctx id_str;
  }

let hints_of_rel_assert_instr (ctx : Ctx.t) (id_rel : Il.id) (expect : bool) :
    Pl.Annot.hints =
  if expect then
    { Pl.Annot.empty with prose_true = Ctx.find_prose_true_rel ctx id_rel.it }
  else
    { Pl.Annot.empty with prose_false = Ctx.find_prose_false_rel ctx id_rel.it }

let hints_of_rel_instr (ctx : Ctx.t) (id_rel : Il.id) : Pl.Annot.hints =
  let id_str = id_rel.it in
  {
    Pl.Annot.empty with
    prose_in = Ctx.find_prose_in_rel ctx id_str;
    prose_out = lookup_prose_out_rel ctx id_str;
    rel_inputs = Ctx.find_rel_inputs ctx id_str;
  }

let hints_of_result_instr (ctx : Ctx.t) : Pl.Annot.hints =
  match Ctx.current_rel ctx with
  | None -> Pl.Annot.empty
  | Some id_str ->
      { Pl.Annot.empty with prose_out = lookup_prose_out_rel ctx id_str }

let hints_of_rel_def (ctx : Ctx.t) (id_rel : Il.id) (inputs : int list) :
    Pl.Annot.hints =
  let id_str = id_rel.it in
  let prose_out =
    Ctx.find_prose_out_rel ctx id_str
    |> Option.map (fun alter -> Hints.Alter.realign alter inputs)
  in
  {
    Pl.Annot.empty with
    prose = Ctx.find_prose_rel ctx id_str;
    prose_in = Ctx.find_prose_in_rel ctx id_str;
    prose_out;
    prose_true = Ctx.find_prose_true_rel ctx id_str;
    prose_false = Ctx.find_prose_false_rel ctx id_str;
  }

let hints_of_func_def (ctx : Ctx.t) (id_func : Il.id) : Pl.Annot.hints =
  let id_str = id_func.it in
  {
    Pl.Annot.empty with
    prose = Ctx.find_prose_func ctx id_str;
    prose_in = Ctx.find_prose_in_func ctx id_str;
    prose_true = Ctx.find_prose_true_func ctx id_str;
    prose_false = Ctx.find_prose_false_func ctx id_str;
  }

let rec annotate_exp (ctx : Ctx.t) (exp : Sl.exp) : Pl.exp =
  let exp_inner, hints =
    match exp.it with
    | Il.BoolE b -> (Pl.BoolE b, Pl.Annot.empty)
    | Il.NumE n -> (Pl.NumE n, Pl.Annot.empty)
    | Il.TextE t -> (Pl.TextE t, Pl.Annot.empty)
    | Il.VarE id -> (Pl.VarE id, Pl.Annot.empty)
    | Il.UnE (op, optyp, e) ->
        (Pl.UnE (op, optyp, annotate_exp ctx e), Pl.Annot.empty)
    | Il.BinE (op, optyp, e1, e2) ->
        ( Pl.BinE (op, optyp, annotate_exp ctx e1, annotate_exp ctx e2),
          Pl.Annot.empty )
    | Il.CmpE (op, optyp, e1, e2) ->
        ( Pl.CmpE (op, optyp, annotate_exp ctx e1, annotate_exp ctx e2),
          Pl.Annot.empty )
    | Il.UpCastE (typ, e) ->
        (Pl.UpCastE (typ, annotate_exp ctx e), Pl.Annot.empty)
    | Il.DownCastE (typ, e) ->
        (Pl.DownCastE (typ, annotate_exp ctx e), Pl.Annot.empty)
    | Il.SubE (e, typ) -> (Pl.SubE (annotate_exp ctx e, typ), Pl.Annot.empty)
    | Il.MatchE (e, pat) -> (Pl.MatchE (annotate_exp ctx e, pat), Pl.Annot.empty)
    | Il.TupleE es ->
        (Pl.TupleE (List.map (annotate_exp ctx) es), Pl.Annot.empty)
    | Il.CaseE notexp ->
        let mixop = Il.Mixfix.to_mixop notexp in
        let hints =
          { Pl.Annot.empty with prose = Ctx.find_prose_typcase ctx ~mixop }
        in
        (Pl.CaseE (annotate_notexp ctx notexp), hints)
    | Il.StrE fields ->
        ( Pl.StrE (List.map (fun (a, e) -> (a, annotate_exp ctx e)) fields),
          Pl.Annot.empty )
    | Il.OptE eo -> (Pl.OptE (Option.map (annotate_exp ctx) eo), Pl.Annot.empty)
    | Il.ListE es -> (Pl.ListE (List.map (annotate_exp ctx) es), Pl.Annot.empty)
    | Il.ConsE (e1, e2) ->
        (Pl.ConsE (annotate_exp ctx e1, annotate_exp ctx e2), Pl.Annot.empty)
    | Il.CatE (e1, e2) ->
        (Pl.CatE (annotate_exp ctx e1, annotate_exp ctx e2), Pl.Annot.empty)
    | Il.MemE (e1, e2) ->
        (Pl.MemE (annotate_exp ctx e1, annotate_exp ctx e2), Pl.Annot.empty)
    | Il.LenE e -> (Pl.LenE (annotate_exp ctx e), Pl.Annot.empty)
    | Il.DotE (e, atom) -> (Pl.DotE (annotate_exp ctx e, atom), Pl.Annot.empty)
    | Il.IdxE (e1, e2) ->
        (Pl.IdxE (annotate_exp ctx e1, annotate_exp ctx e2), Pl.Annot.empty)
    | Il.SliceE (e1, e2, e3) ->
        ( Pl.SliceE
            (annotate_exp ctx e1, annotate_exp ctx e2, annotate_exp ctx e3),
          Pl.Annot.empty )
    | Il.UpdE (e1, path, e2) ->
        ( Pl.UpdE
            (annotate_exp ctx e1, annotate_path ctx path, annotate_exp ctx e2),
          Pl.Annot.empty )
    | Il.CallE (id, targs, args) ->
        ( Pl.CallE (id, targs, List.map (annotate_arg ctx) args),
          hints_of_call_exp ctx id )
    | Il.IterE (e, iterexp) ->
        (Pl.IterE (annotate_exp ctx e, iterexp), Pl.Annot.empty)
  in
  wrap_exp (exp_inner $$ (exp.at, exp.note)) hints

and annotate_notexp (ctx : Ctx.t) (notexp : Sl.notexp) : Pl.notexp =
  Il.Mixfix.map (annotate_exp ctx) notexp

and annotate_path (ctx : Ctx.t) (path : Sl.path) : Pl.path =
  let path_inner : Pl.path' =
    match path.it with
    | Il.RootP -> Pl.RootP
    | Il.IdxP (p, e) -> Pl.IdxP (annotate_path ctx p, annotate_exp ctx e)
    | Il.SliceP (p, e1, e2) ->
        Pl.SliceP (annotate_path ctx p, annotate_exp ctx e1, annotate_exp ctx e2)
    | Il.DotP (p, atom) -> Pl.DotP (annotate_path ctx p, atom)
  in
  path_inner $$ (path.at, path.note)

and annotate_arg (ctx : Ctx.t) (arg : Sl.arg) : Pl.arg =
  let arg_inner : Pl.arg' =
    match arg.it with
    | Il.ExpA e -> Pl.ExpA (annotate_exp ctx e)
    | Il.DefA id -> Pl.DefA id
  in
  arg_inner $ arg.at

let annotate_guard (ctx : Ctx.t) (g : Ll.guard) : Pl.guard =
  match g with
  | Ll.BoolG b -> Pl.BoolG b
  | Ll.CmpG (op, optyp, e) -> Pl.CmpG (op, optyp, annotate_exp ctx e)
  | Ll.SubG typ -> Pl.SubG typ
  | Ll.MatchG pat -> Pl.MatchG pat
  | Ll.MemG e -> Pl.MemG (annotate_exp ctx e)

let rec annotate_instr (ctx : Ctx.t) (instr : Ll.instr) : Pl.instr =
  let instr_inner, hints =
    match instr.it with
    | Ll.IfI (cond, iterexps, block, phantom_opt) ->
        ( Pl.IfI
            ( annotate_exp ctx cond,
              iterexps,
              annotate_block ctx block,
              phantom_opt ),
          Pl.Annot.empty )
    | Ll.RelAssertI { call; expect; iterexps; block; phantom } ->
        ( Pl.RelAssertI
            {
              call =
                {
                  Pl.relid = call.relid;
                  notexp = annotate_notexp ctx call.notexp;
                };
              expect;
              iterexps;
              block = annotate_block ctx block;
              phantom;
            },
          hints_of_rel_assert_instr ctx call.relid expect )
    | Ll.CaseI (exp, cases, phantom_opt) ->
        ( Pl.CaseI
            ( annotate_exp ctx exp,
              List.map (annotate_case ctx) cases,
              phantom_opt ),
          Pl.Annot.empty )
    | Ll.OtherwiseI block -> (
        let block = annotate_block ctx block in
        match block with
        | [ single ] -> (Pl.OtherwiseI single, Pl.Annot.empty)
        | _ ->
            ( Pl.OtherwiseI (bare_instr (Pl.TryI [ block ] $ instr.at)),
              Pl.Annot.empty ))
    | Ll.TryI arms ->
        (Pl.TryI (List.map (annotate_block ctx) arms), Pl.Annot.empty)
    | Ll.LetI (exp_l, exp_r, iterexps) ->
        let exp_l_pl = annotate_exp ctx exp_l in
        let exp_r_pl = annotate_exp ctx exp_r in
        let hints =
          match exp_l_pl.node.it with
          | Pl.CaseE notexp ->
              let mixop = Il.Mixfix.to_mixop notexp in
              {
                Pl.Annot.empty with
                prose_fields = Ctx.find_fields_typcase ctx ~mixop;
              }
          | _ -> Pl.Annot.empty
        in
        (Pl.LetI (exp_l_pl, exp_r_pl, iterexps), hints)
    | Ll.RelI { call; iterexps } ->
        ( Pl.RelI
            {
              call =
                {
                  Pl.relid = call.relid;
                  notexp = annotate_notexp ctx call.notexp;
                };
              iterexps;
            },
          hints_of_rel_instr ctx call.relid )
    | Ll.ResultI exps ->
        ( Pl.ResultI (List.map (annotate_exp ctx) exps),
          hints_of_result_instr ctx )
    | Ll.ReturnI exp -> (Pl.ReturnI (annotate_exp ctx exp), Pl.Annot.empty)
    | Ll.DebugI exp -> (Pl.DebugI (annotate_exp ctx exp), Pl.Annot.empty)
  in
  wrap_instr (instr_inner $ instr.at) hints

and annotate_case (ctx : Ctx.t) (case : Ll.case) : Pl.case =
  let g, block = case in
  (annotate_guard ctx g, annotate_block ctx block)

and annotate_block (ctx : Ctx.t) (block : Ll.block) : Pl.block =
  List.map (annotate_instr ctx) block

let annotate_elseblock_opt (ctx : Ctx.t) (elseblock_opt : Ll.elseblock option) :
    Pl.elseblock option =
  Option.map (annotate_block ctx) elseblock_opt

let annotate_def (ctx : Ctx.t) (def : Ll.def) : Pl.def =
  let def_inner, hints =
    match def.it with
    | Ll.TypD (id, tparams, deftyp) ->
        (Pl.TypD (id, tparams, deftyp), Pl.Annot.empty)
    | Ll.RelD (id, sig_, exps, block, elseblock_opt) ->
        let inputs = snd sig_ in
        let ctx_rel = Ctx.enter_rel ctx id.it in
        ( Pl.RelD
            ( id,
              sig_,
              List.map (annotate_exp ctx_rel) exps,
              annotate_block ctx_rel block,
              annotate_elseblock_opt ctx_rel elseblock_opt ),
          hints_of_rel_def ctx id inputs )
    | Ll.BuiltinDecD (id, tparams, args) ->
        ( Pl.BuiltinDecD (id, tparams, List.map (annotate_arg ctx) args),
          hints_of_func_def ctx id )
    | Ll.DecD (id, tparams, args, block, elseblock_opt) ->
        let ctx_func = Ctx.enter_func ctx id.it in
        ( Pl.DecD
            ( id,
              tparams,
              List.map (annotate_arg ctx_func) args,
              annotate_block ctx_func block,
              annotate_elseblock_opt ctx_func elseblock_opt ),
          hints_of_func_def ctx id )
  in
  wrap_def (def_inner $ def.at) hints

let annotate_spec (henv : Hints.Henv.t) (spec : Ll.spec) : Pl.spec =
  let ctx = Ctx.init henv in
  List.map (annotate_def ctx) spec
