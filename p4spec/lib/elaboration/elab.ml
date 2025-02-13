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

(* Iteration *)

let elab_iter (iter : iter) : Il.Ast.iter =
  match iter with Opt -> Il.Ast.Opt | List -> Il.Ast.List

(* Types *)

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

(* Definitions *)

let rec elab_def (ctx : Ctx.t) (def : def) : Ctx.t * Il.Ast.def option =
  let wrap_some (ctx, def) = (ctx, Some def) in
  let wrap_none ctx = (ctx, None) in
  match def.it with
  | SynD (id, tparams) -> elab_syn_def ctx id tparams |> wrap_none
  | TypD (id, tparams, deftyp, _hints) ->
      elab_typ_def ctx id tparams deftyp |> wrap_some
  | _ -> failwith "todo elab_def"

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

(* Spec *)

let elab_spec (spec : spec) : Il.Ast.spec =
  let ctx = Ctx.empty in
  let _ctx, spec_il = elab_defs ctx spec in
  spec_il
