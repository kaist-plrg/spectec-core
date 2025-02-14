open Xl
open El.Ast
open Util.Error
open Util.Source

(* Checks *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at "elab" msg

let distinct (eq : 'a -> 'a -> bool) (xs : 'a list) : bool =
  let rec distinct' xs =
    match xs with
    | [] -> true
    | x :: xs -> if List.exists (eq x) xs then false else distinct' xs
  in
  distinct' xs

(* Parentheses handling *)

let rec unparen_plaintyp (plaintyp : plaintyp) : plaintyp =
  match plaintyp.it with
  | ParenT plaintyp -> unparen_plaintyp plaintyp
  | _ -> plaintyp

let unparen_nottyp (nottyp : nottyp) : nottyp =
  match nottyp.it with
  | PlainT { it = ParenT plaintyp; at; _ } ->
      let plaintyp = unparen_plaintyp plaintyp in
      PlainT plaintyp $ at
  | _ -> nottyp

let rec unparen_exp (exp : exp) : exp =
  match exp.it with ParenE exp -> unparen_exp exp | _ -> exp

(* Iteration *)

let elab_iter (iter : iter) : Il.Ast.iter =
  match iter with Opt -> Il.Ast.Opt | List -> Il.Ast.List

(* Types *)

type kind =
  [ `Plain
  | `Notation of nottyp
  | `Struct of typfield list
  | `Variant of typcase list ]

let kind_of_typ (ctx : Ctx.t) (typ_il : Il.Ast.typ) : kind =
  match typ_il.it with
  | VarT (tid, _) -> (
      let td = Ctx.find_typdef_opt ctx tid.it |> Option.get in
      match td with
      | Param | Defining _ -> `Plain
      | Defined (_, deftyp) -> (
          match deftyp.it with
          | NotationT { it = PlainT _; _ } -> `Plain
          | NotationT nottyp -> `Notation nottyp
          | StructT typfields -> `Struct typfields
          | VariantT typcases -> `Variant typcases))
  | _ -> `Plain

(* Plain types *)

let rec elab_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) : Il.Ast.typ =
  let typ_il = elab_plaintyp' ctx plaintyp.it in
  typ_il $ plaintyp.at

and elab_plaintyp' (ctx : Ctx.t) (plaintyp : plaintyp') : Il.Ast.typ' =
  match plaintyp with
  | VarT (tid, targs) ->
      let td_opt = Ctx.find_typdef_opt ctx tid.it in
      check (Option.is_some td_opt) tid.at "type not defined";
      let typs_il = List.map (elab_plaintyp ctx) targs in
      let td = Option.get td_opt in
      let tparams = Typedef.get_tparams td in
      check
        (List.length tparams = List.length targs)
        tid.at "type arguments do not match";
      Il.Ast.VarT (tid, typs_il)
  | BoolT -> Il.Ast.BoolT
  | NumT numtyp -> Il.Ast.NumT numtyp
  | TextT -> Il.Ast.TextT
  | ParenT plaintyp -> elab_plaintyp' ctx plaintyp.it
  | TupT plaintyps ->
      let typs_il = List.map (elab_plaintyp ctx) plaintyps in
      Il.Ast.TupT typs_il
  | IterT (plaintyp, iter) ->
      let typ_il = elab_plaintyp ctx plaintyp in
      let iter_il = elab_iter iter in
      Il.Ast.IterT (typ_il, iter_il)

(* Notation types *)

and elab_nottyp (ctx : Ctx.t) (nottyp : nottyp) : Il.Ast.nottyp =
  let nottyp_il = elab_nottyp' ctx nottyp.it in
  nottyp_il $ nottyp.at

and elab_nottyp' (ctx : Ctx.t) (nottyp : nottyp') : Il.Ast.nottyp' =
  match nottyp with
  | PlainT plaintyp ->
      let mixop = [ []; [] ] in
      let typ_il = elab_plaintyp ctx plaintyp in
      (mixop, [ typ_il ])
  | AtomT atom ->
      let mixop = [ [ atom ] ] in
      let typs_il = [] in
      (mixop, typs_il)
  | SeqT [] ->
      let mixop = [ [] ] in
      let typs_il = [] in
      (mixop, typs_il)
  | SeqT (nottyp :: nottyps) ->
      let mixop_h, typs_il_h = elab_nottyp' ctx nottyp.it in
      let mixop_t, typs_il_t = elab_nottyp' ctx (SeqT nottyps) in
      let mixop = Mixop.merge mixop_h mixop_t in
      let typs_il = typs_il_h @ typs_il_t in
      (mixop, typs_il)
  | InfixT (nottyp_l, atom, nottyp_r) ->
      let mixop_l, typs_il_l = elab_nottyp' ctx nottyp_l.it in
      let mixop_r, typs_il_r = elab_nottyp' ctx nottyp_r.it in
      let mixop_l = Mixop.merge mixop_l [ [ atom ] ] in
      let mixop = Mixop.merge mixop_l mixop_r in
      let typs_il = typs_il_l @ typs_il_r in
      (mixop, typs_il)
  | BrackT (atom_l, nottyp, atom_r) ->
      let mixop, typs_il = elab_nottyp' ctx nottyp.it in
      let mixop_l = Mixop.merge [ [ atom_l ] ] mixop in
      let mixop = Mixop.merge mixop_l [ [ atom_r ] ] in
      (mixop, typs_il)

(* Definition types *)

and elab_deftyp (ctx : Ctx.t) (deftyp : deftyp) : Il.Ast.deftyp =
  match deftyp.it with
  | NotationT nottyp -> elab_typ_def_notation ctx nottyp
  | StructT typfields -> elab_typ_def_struct ctx deftyp.at typfields
  | VariantT typcases -> elab_typ_def_variant ctx deftyp.at typcases

(* Notation type definitions *)

and elab_typ_def_notation (ctx : Ctx.t) (nottyp : nottyp) : Il.Ast.deftyp =
  match nottyp.it with
  | PlainT plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      Il.Ast.AliasT typ_il $ nottyp.at
  | _ ->
      let nottyp_il = elab_nottyp ctx nottyp in
      Il.Ast.NotationT nottyp_il $ nottyp.at

(* Struct type definitions *)

and elab_typ_def_struct (ctx : Ctx.t) (at : region) (typfields : typfield list)
    : Il.Ast.deftyp =
  let typfields_il = List.map (elab_typfield ctx) typfields in
  let deftyp_il = Il.Ast.StructT typfields_il in
  deftyp_il $ at

and elab_typfield (ctx : Ctx.t) (typfield : typfield) : Il.Ast.typfield =
  let atom, plaintyps, _hints = typfield in
  let typs_il = List.map (elab_plaintyp ctx) plaintyps in
  (atom, typs_il)

(* Variant type definitions *)

and elab_typcase (ctx : Ctx.t) (typcase : typcase) : Il.Ast.typcase =
  let nottyp, _hints = typcase in
  elab_nottyp ctx nottyp

and elab_typ_def_variant (ctx : Ctx.t) (at : region) (typcases : typcase list) :
    Il.Ast.deftyp =
  let typcases_il = List.map (elab_typcase ctx) typcases in
  let mixops = typcases_il |> List.map it |> List.map fst in
  check (distinct Mixop.eq mixops) no_region "cases are ambiguous";
  let deftyp_il = Il.Ast.VariantT typcases_il in
  deftyp_il $ at

(* Expressions *)

and elab_exp (ctx : Ctx.t) (plaintyp : plaintyp) (exp : exp) : Il.Ast.exp =
  let exp = unparen_exp exp in
  let typ_il = elab_plaintyp ctx plaintyp in
  let kind = kind_of_typ ctx typ_il in
  match kind with
  | `Plain -> elab_exp_plain ctx plaintyp exp
  | `Notation nottyp ->
      let notexp_il = elab_exp_not ctx nottyp exp in
      Il.Ast.CaseE notexp_il $$ (exp.at, typ_il.it)
  | `Variant typcases ->
      let notexp_il = elab_exp_variant ctx typcases exp in
      Il.Ast.CaseE notexp_il $$ (exp.at, typ_il.it)
  | _ ->
      Format.printf "todo elab_exp\n";
      assert false

(* Plain expressions *)

and elab_exp_plain (ctx : Ctx.t) (plaintyp : plaintyp) (exp : exp) : Il.Ast.exp
    =
  match exp.it with
  | BoolE b ->
      let exp_il = Il.Ast.BoolE b in
      let typ_il = Il.Ast.BoolT in
      exp_il $$ (exp.at, typ_il)
  | NumE (_, num) ->
      let exp_il = Il.Ast.NumE num in
      let typ_il = Il.Ast.NumT (Num.to_typ num) in
      exp_il $$ (exp.at, typ_il)
  | TextE s ->
      let exp_il = Il.Ast.TextE s in
      let typ_il = Il.Ast.TextT in
      exp_il $$ (exp.at, typ_il)
  | ParenE exp ->
      let exp_il = elab_exp ctx plaintyp exp in
      let typ_il = exp_il.note in
      exp_il.it $$ (exp.at, typ_il)
  | _ ->
      Format.printf "todo elab_exp_plain %s\n" (El.Print.string_of_exp exp);
      assert false

(* Notation expressions *)

and elab_exp_not (ctx : Ctx.t) (nottyp : nottyp) (exp : exp) : Il.Ast.notexp =
  let exp = unparen_exp exp in
  let nottyp = unparen_nottyp nottyp in
  match (nottyp.it, exp.it) with
  | PlainT plaintyp, _ ->
      let mixop = [ []; [] ] in
      let exp_il = elab_exp ctx plaintyp exp in
      (mixop, [ exp_il ])
  | AtomT atom_t, AtomE atom_e ->
      check (atom_t.it = atom_e.it) exp.at "atoms do not match";
      let mixop = [ [ atom_t ] ] in
      (mixop, [])
  | SeqT [], SeqE [] ->
      let mixop = [ [] ] in
      let exps_il = [] in
      (mixop, exps_il)
  | SeqT (nottyp :: nottyps), SeqE (exp :: exps) ->
      let mixop_h, exps_il_h = elab_exp_not ctx nottyp exp in
      let mixop_t, exps_il_t =
        elab_exp_not ctx (SeqT nottyps $ nottyp.at) (SeqE exps $ exp.at)
      in
      let mixop = Mixop.merge mixop_h mixop_t in
      let exps_il = exps_il_h @ exps_il_t in
      (mixop, exps_il)
  | SeqT (_ :: _), SeqE [] -> error exp.at "elab" "omitted sequence tail"
  | SeqT [], SeqE (_ :: _) -> error exp.at "elab" "expression is not empty"
  | InfixT (nottyp_l, atom_t, nottyp_r), InfixE (exp_l, atom_e, exp_r) ->
      check (atom_t.it = atom_e.it) exp.at "atoms do not match";
      let mixop_l, exps_il_l = elab_exp_not ctx nottyp_l exp_l in
      let mixop_r, exps_il_r = elab_exp_not ctx nottyp_r exp_r in
      let mixop_l = Mixop.merge mixop_l [ [ atom_t ] ] in
      let mixop = Mixop.merge mixop_l mixop_r in
      let exps_il = exps_il_l @ exps_il_r in
      (mixop, exps_il)
  | BrackT (atom_t_l, nottyp, atom_t_r), BrackE (atom_e_l, exp, atom_e_r) ->
      check (atom_t_l.it = atom_e_l.it) exp.at "left atoms do not match";
      check (atom_t_r.it = atom_e_r.it) exp.at "right atoms do not match";
      let mixop, exps_il = elab_exp_not ctx nottyp exp in
      let mixop_l = Mixop.merge [ [ atom_t_l ] ] mixop in
      let mixop = Mixop.merge mixop_l [ [ atom_t_r ] ] in
      (mixop, exps_il)
  | _ ->
      Format.printf "nottyp: %s\n" (El.Print.string_of_nottyp nottyp);
      Format.printf "exp: %s\n" (El.Print.string_of_exp exp);
      error exp.at "elab" "notation does not match expression"

(* Variant expressions *)

and elab_exp_variant (ctx : Ctx.t) (typcases : typcase list) (exp : exp) :
    Il.Ast.notexp =
  let notexps_il =
    List.filter_map
      (fun (nottyp, _) ->
        try
          let notexp_il = elab_exp_not ctx nottyp exp in
          Some notexp_il
        with _ -> None)
      typcases
  in
  match notexps_il with
  | [ notexp_il ] -> notexp_il
  | [] -> error exp.at "elab" "expression does not match any case"
  | _ -> error exp.at "elab" "expression matches multiple cases"

(* Definitions *)

let rec elab_def (ctx : Ctx.t) (def : def) : Ctx.t * Il.Ast.def option =
  let wrap_some (ctx, def) = (ctx, Some def) in
  let wrap_none ctx = (ctx, None) in
  match def.it with
  | SynD (id, tparams) -> elab_syn_def ctx id tparams |> wrap_none
  | TypD (id, tparams, deftyp, _hints) ->
      elab_typ_def ctx id tparams deftyp |> wrap_some
  | RelD (id, nottyp, _hints) -> elab_rel_def ctx id nottyp |> wrap_some
  | RuleD (id_rel, id_rule, exp, prems) ->
      elab_rule_def ctx def.at id_rel id_rule exp prems |> wrap_none
  | SepD -> ctx |> wrap_none
  | _ ->
      Format.printf "todo elab_def\n";
      assert false

and elab_defs (ctx : Ctx.t) (defs : def list) : Ctx.t * Il.Ast.def list =
  List.fold_left
    (fun (ctx, defs_il) def ->
      let ctx, def_il_opt = elab_def ctx def in
      match def_il_opt with
      | Some def_il -> (ctx, defs_il @ [ def_il ])
      | None -> (ctx, defs_il))
    (ctx, []) defs

(* Syntax definitions *)

and elab_syn_def (ctx : Ctx.t) (id : id) (tparams : tparam list) : Ctx.t =
  let td = Typedef.Defining tparams in
  Ctx.add_typdef ctx id.it td

(* Type definitions *)

and elab_typ_def (ctx : Ctx.t) (id : id) (tparams : tparam list)
    (deftyp : deftyp) : Ctx.t * Il.Ast.def =
  let td_opt = Ctx.find_typdef_opt ctx id.it in
  let ctx =
    match td_opt with
    | Some (Typedef.Defining tparams_defining) ->
        let tparams = List.map it tparams in
        let tparams_defining = List.map it tparams_defining in
        check
          (List.length tparams = List.length tparams_defining
          && List.for_all2 ( = ) tparams tparams_defining)
          id.at "type parameters do not match";
        ctx
    | None ->
        let td = Typedef.Defining tparams in
        Ctx.add_typdef ctx id.it td
    | _ -> error id.at "elab" "type was already defined"
  in
  let ctx_local = Ctx.add_tparams ctx tparams in
  let deftyp_il = elab_deftyp ctx_local deftyp in
  let def_il = Il.Ast.TypD (id, tparams, deftyp_il) $ deftyp.at in
  let td = Typedef.Defined (tparams, deftyp) in
  let ctx = Ctx.update_typdef ctx id.it td in
  (ctx, def_il)

(* Relation definitions *)

and elab_rel_def (ctx : Ctx.t) (id : id) (nottyp : nottyp) : Ctx.t * Il.Ast.def
    =
  let nottyp_il = elab_nottyp ctx nottyp in
  let def_il = Il.Ast.RelD (id, nottyp_il, []) $ nottyp.at in
  let ctx = Ctx.add_rel ctx id.it nottyp in
  (ctx, def_il)

(* Rule definitions *)

and elab_rule_def (ctx : Ctx.t) (at : region) (id_rel : id) (id_rule : id)
    (exp : exp) (_prems : prem list) : Ctx.t =
  let nottyp_opt = Ctx.find_rel_opt ctx id_rel.it in
  check (Option.is_some nottyp_opt) id_rel.at "relation not defined";
  let nottyp = Option.get nottyp_opt in
  let notexp_il = elab_exp_not ctx nottyp exp in
  let rule = (id_rule, notexp_il, []) $ at in
  Ctx.add_rule ctx id_rel.it rule

(* Spec *)

let elab_spec (spec : spec) : Il.Ast.spec =
  let ctx = Ctx.empty in
  let _ctx, spec_il = elab_defs ctx spec in
  spec_il
