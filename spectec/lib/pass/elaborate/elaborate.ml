open Common.Domain
open Common.Source
open Lang.Xl
open Lang.El
module El = Lang.El
module Il = Lang.Il
open Envs.Make
open Attempt
open Diagnostic
open Ctx
module Fresh = Dataflow.Fresh

(* Checks *)

let distinct (eq : 'a -> 'a -> bool) (xs : 'a list) : bool =
  let rec distinct' xs =
    match xs with
    | [] -> true
    | x :: xs -> if List.exists (eq x) xs then false else distinct' xs
  in
  distinct' xs

let groupby (eq : 'a -> 'a -> bool) (xs : 'a list) : 'a list list =
  let rec groupby' acc xs =
    match xs with
    | [] -> List.rev acc
    | x :: xs ->
        let same, rest = List.partition (eq x) xs in
        groupby' ((x :: same) :: acc) rest
  in
  groupby' [] xs

(* Identifiers *)

let valid_tid (id : id) = id.it = (Var.strip_var_suffix id).it

(* Re-tag the checks synthesized from a rule/clause input pattern as guards. *)
let rec as_guard (prem : Il.prem) : Il.prem =
  let guard =
    match prem.it with
    | Il.IfPr { cond; _ } -> Il.IfPr { cond; role = Il.Guard } $ prem.at
    | Il.IterPr (prem_inner, iterexp) ->
        Il.IterPr (as_guard prem_inner, iterexp) $ prem.at
    | _ -> prem
  in
  { guard with prov = Some Il.Synthesized }

(* Iteration elaboration *)

let elab_iter (iter : iter) : Il.iter =
  match iter with Opt -> Il.Opt | List -> Il.List

(* Type expansion, equivalence, and subtyping *)

module Types = struct
  let rec expand_typ (tdenv : Ctx.TDEnv.t) (typ : Il.typ) : Il.typ =
    match typ.it with
    | Il.VarT { synid = tid; targs } -> (
        let td_opt = TIdMap.find_opt tid tdenv in
        match td_opt with
        | Some (Typdef.Defined (tparams, deftyp)) -> (
            match deftyp.it with
            | Il.PlainT _ when List.length targs <> List.length tparams ->
                Common.InternalError.disallowed typ.at
                  ("type variable " ^ tid.it ^ " expects "
                  ^ string_of_int (List.length tparams)
                  ^ " type arguments, but got "
                  ^ string_of_int (List.length targs))
            | Il.PlainT typ ->
                let theta = List.combine tparams targs |> TIdMap.of_list in
                let typ = Envs.Il.Typ.subst_typ theta typ in
                expand_typ tdenv typ
            | _ -> typ)
        | Some _ -> typ
        | None ->
            Common.InternalError.disallowed typ.at
              ("type variable " ^ tid.it ^ " is not defined"))
    | _ -> typ

  let rec equiv_typ (tdenv : Ctx.TDEnv.t) (typ_a : Il.typ) (typ_b : Il.typ) :
      bool =
    let typ_a = expand_typ tdenv typ_a in
    let typ_b = expand_typ tdenv typ_b in
    match (typ_a.it, typ_b.it) with
    | Il.BoolT, Il.BoolT -> true
    | Il.NumT numtyp_a, Il.NumT numtyp_b -> Num.equiv numtyp_a numtyp_b
    | Il.TextT, Il.TextT -> true
    | ( Il.VarT { synid = tid_a; targs = targs_a },
        Il.VarT { synid = tid_b; targs = targs_b } ) ->
        tid_a.it = tid_b.it
        && List.length targs_a = List.length targs_b
        && List.for_all2 (equiv_typ tdenv) targs_a targs_b
    | Il.TupleT typs_a, Il.TupleT typs_b ->
        List.length typs_a = List.length typs_b
        && List.for_all2 (equiv_typ tdenv) typs_a typs_b
    | ( Il.IterT { typ = typ_a; iter = iter_a },
        Il.IterT { typ = typ_b; iter = iter_b } ) ->
        equiv_typ tdenv typ_a typ_b && iter_a = iter_b
    | _ -> false

  and equiv_nottyp (tdenv : Ctx.TDEnv.t) (nottyp_a : Il.nottyp)
      (nottyp_b : Il.nottyp) : bool =
    Il.Mixfix.eq ~eq_arg:(equiv_typ tdenv) nottyp_a.it nottyp_b.it

  and equiv_param (tdenv : Ctx.TDEnv.t) (param_a : Il.param)
      (param_b : Il.param) : bool =
    match (param_a.it, param_b.it) with
    | Il.ExpP typ_a, Il.ExpP typ_b -> equiv_typ tdenv typ_a typ_b
    | ( Il.DefP { tparams = tparams_a; params = params_a; typ = typ_a; _ },
        Il.DefP { tparams = tparams_b; params = params_b; typ = typ_b; _ } ) ->
        equiv_functyp tdenv param_a.at tparams_a params_a typ_a tparams_b
          params_b typ_b
    | _ -> false

  and equiv_functyp (tdenv : Ctx.TDEnv.t) (at : region)
      (tparams_a : Il.tparam list) (params_a : Il.param list) (typ_a : Il.typ)
      (tparams_b : Il.tparam list) (params_b : Il.param list) (typ_b : Il.typ) :
      bool =
    check
      (List.length tparams_a = List.length tparams_b)
      no_region "type parameters do not match"
      ~code:Functyp_tparam_arity_mismatch;
    let tdenv, theta_a, theta_b =
      List.fold_left2
        (fun (tdenv, theta_a, theta_b) tparam_a tparam_b ->
          let tid_fresh = "__FRESH" ^ string_of_int (fresh ()) $ no_region in
          let typ_fresh =
            Il.VarT { synid = tid_fresh; targs = [] } $ no_region
          in
          let tdenv = TDEnv.add tid_fresh Typdef.Param tdenv in
          let theta_a = TIdMap.add tparam_a typ_fresh theta_a in
          let theta_b = TIdMap.add tparam_b typ_fresh theta_b in
          (tdenv, theta_a, theta_b))
        (tdenv, TIdMap.empty, TIdMap.empty)
        tparams_a tparams_b
    in
    check
      (List.length params_a = List.length params_b)
      at "parameters do not match" ~code:Functyp_param_arity_mismatch;
    let params_a = Envs.Il.Typ.subst_params theta_a params_a in
    let params_b = Envs.Il.Typ.subst_params theta_b params_b in
    let typ_a = Envs.Il.Typ.subst_typ theta_a typ_a in
    let typ_b = Envs.Il.Typ.subst_typ theta_b typ_b in
    List.for_all2 (equiv_param tdenv) params_a params_b
    && equiv_typ tdenv typ_a typ_b

  let rec sub_typ (tdenv : Ctx.TDEnv.t) (typ_a : Il.typ) (typ_b : Il.typ) : bool
      =
    equiv_typ tdenv typ_a typ_b || sub_typ' tdenv typ_a typ_b

  and sub_typ' (tdenv : Ctx.TDEnv.t) (typ_a : Il.typ) (typ_b : Il.typ) : bool =
    let typ_a = expand_typ tdenv typ_a in
    let typ_b = expand_typ tdenv typ_b in
    match (typ_a.it, typ_b.it) with
    | Il.NumT numtyp_a, Il.NumT numtyp_b -> Num.sub numtyp_a numtyp_b
    | ( Il.VarT { synid = tid_a; targs = targs_a },
        Il.VarT { synid = tid_b; targs = targs_b } ) -> (
        let td_opt_a = TDEnv.find_opt tid_a tdenv in
        let td_opt_b = TDEnv.find_opt tid_b tdenv in
        match (td_opt_a, td_opt_b) with
        | ( Some (Typdef.Defined (tparams_a, deftyp_a)),
            Some (Typdef.Defined (tparams_b, deftyp_b)) ) -> (
            match (deftyp_a.it, deftyp_b.it) with
            | Il.VariantT typcases_a, Il.VariantT typcases_b ->
                let theta_a =
                  List.combine tparams_a targs_a |> TIdMap.of_list
                in
                let theta_b =
                  List.combine tparams_b targs_b |> TIdMap.of_list
                in
                let nottyps_a =
                  typcases_a
                  |> List.map (fun typcase -> typcase.Il.notation)
                  |> List.map (Envs.Il.Typ.subst_nottyp theta_a)
                in
                let nottyps_b =
                  typcases_b
                  |> List.map (fun typcase -> typcase.Il.notation)
                  |> List.map (Envs.Il.Typ.subst_nottyp theta_b)
                in
                List.for_all
                  (fun nottyp_a ->
                    List.exists (equiv_nottyp tdenv nottyp_a) nottyps_b)
                  nottyps_a
            | _, _ -> false)
        | _ -> false)
    | Il.TupleT typs_a, Il.TupleT typs_b ->
        List.length typs_a = List.length typs_b
        && List.for_all2 (sub_typ tdenv) typs_a typs_b
    | ( Il.IterT { typ = typ_a; iter = iter_a },
        Il.IterT { typ = typ_b; iter = iter_b } )
      when iter_a = iter_b ->
        sub_typ tdenv typ_a typ_b
    | ( Il.IterT { typ = typ_a; iter = Il.Opt },
        Il.IterT { typ = typ_b; iter = Il.List } ) ->
        sub_typ tdenv typ_a typ_b
    | _, Il.IterT { typ = typ_b; iter = Il.Opt } -> sub_typ tdenv typ_a typ_b
    | _, Il.IterT { typ = typ_b; iter = Il.List } -> sub_typ tdenv typ_a typ_b
    | _ -> false
end

(* Types *)

(* Type destructuring *)

let as_text_typ (ctx : Ctx.t) (typ_il : Il.typ) : unit attempt =
  let typ_il = Types.expand_typ ctx.tdenv typ_il in
  match typ_il.it with
  | TextT -> Ok ()
  | _ -> fail typ_il.at "cannot destruct type as text"

let as_iter_typ (ctx : Ctx.t) (typ_il : Il.typ) : (Il.typ * Il.iter) attempt =
  let typ_il = Types.expand_typ ctx.tdenv typ_il in
  match typ_il.it with
  | IterT { typ = typ_il; iter } -> Ok (typ_il, iter)
  | _ -> fail typ_il.at "cannot destruct type as an iteration"

let as_tuple_typ (ctx : Ctx.t) (typ_il : Il.typ) : Il.typ list attempt =
  let typ_il = Types.expand_typ ctx.tdenv typ_il in
  match typ_il.it with
  | TupleT typs_il -> Ok typs_il
  | _ -> fail typ_il.at "cannot destruct type as a tuple"

let as_list_typ (ctx : Ctx.t) (typ_il : Il.typ) : Il.typ attempt =
  let typ_il = Types.expand_typ ctx.tdenv typ_il in
  match typ_il.it with
  | IterT { typ = typ_il; iter = List } -> Ok typ_il
  | _ -> fail typ_il.at "cannot destruct type as a list"

let as_struct_typ (ctx : Ctx.t) (typ_il : Il.typ) : Il.typfield list attempt =
  let typ_il = Types.expand_typ ctx.tdenv typ_il in
  match typ_il.it with
  | VarT { synid = tid; _ } -> (
      let td_opt = Ctx.find_typdef_opt ctx tid in
      match td_opt with
      | Some (Defined (_, deftyp)) -> (
          match deftyp.it with
          | StructT typfields_il -> Ok typfields_il
          | _ -> fail typ_il.at "cannot destruct type as a struct")
      | _ -> fail typ_il.at "cannot destruct type as a struct")
  | _ -> fail typ_il.at "cannot destruct type as a struct"

(* Elaboration of plain types *)

let rec elab_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) : Il.typ =
  let typ_il = elab_plaintyp' ctx plaintyp.it in
  typ_il $ plaintyp.at

and elab_plaintyp' (ctx : Ctx.t) (plaintyp : plaintyp') : Il.typ' =
  match plaintyp with
  | BoolT -> Il.BoolT
  | NumT numtyp -> Il.NumT numtyp
  | TextT -> Il.TextT
  | VarT (tid, targs) ->
      let td = Ctx.find_typdef ctx tid in
      let tparams = Typdef.get_tparams td in
      check
        (List.length tparams = List.length targs)
        tid.at "type arguments do not match" ~code:Vart_targ_arity_mismatch;
      let targs_il = List.map (elab_plaintyp ctx) targs in
      Il.VarT { synid = tid; targs = targs_il }
  | ParenT plaintyp -> elab_plaintyp' ctx plaintyp.it
  | TupleT plaintyps ->
      let typs_il = List.map (elab_plaintyp ctx) plaintyps in
      Il.TupleT typs_il
  | IterT (plaintyp, iter) ->
      let typ_il = elab_plaintyp ctx plaintyp in
      let iter_il = elab_iter iter in
      Il.IterT { typ = typ_il; iter = iter_il }

(* Elaboration of notation types *)

and elab_nottyp (ctx : Ctx.t) (typ : typ) : Il.nottyp =
  match typ with
  | PlainT plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      [ Il.Mixfix.Arg typ_il ] $ plaintyp.at
  | NotationT nottyp -> (
      match nottyp.it with
      | AtomT atom -> [ Il.Mixfix.Atom atom ] $ nottyp.at
      | SeqT [] -> [] $ nottyp.at
      | SeqT typs ->
          let parts =
            List.map
              (fun typ -> elab_nottyp ctx typ |> it |> Il.Mixfix.split)
              typs
          in
          let mixop_il = List.concat_map fst parts in
          let typs_il = List.concat_map snd parts in
          Il.Mixfix.fill mixop_il typs_il $ nottyp.at
      | InfixT (typ_l, atom, typ_r) ->
          let mixop_il_l, typs_il_l =
            Il.Mixfix.split (elab_nottyp ctx typ_l).it
          in
          let mixop_il_r, typs_il_r =
            Il.Mixfix.split (elab_nottyp ctx typ_r).it
          in
          let mixop_il = mixop_il_l @ [ Il.Mixfix.Atom atom ] @ mixop_il_r in
          let typs_il = typs_il_l @ typs_il_r in
          Il.Mixfix.fill mixop_il typs_il $ nottyp.at
      | BrackT (atom_l, typ, atom_r) ->
          let mixop, typs_il = Il.Mixfix.split (elab_nottyp ctx typ).it in
          let mixop_il =
            [ Il.Mixfix.Atom atom_l ] @ mixop @ [ Il.Mixfix.Atom atom_r ]
          in
          Il.Mixfix.fill mixop_il typs_il $ nottyp.at)

(* Elaboration of definition types *)

and elab_deftyp (ctx : Ctx.t) (id : id) (tparams : tparam list)
    (deftyp : deftyp) : Typdef.t * Il.deftyp =
  match deftyp.it with
  | PlainTD plaintyp -> elab_deftyp_plain ctx tparams plaintyp
  | StructTD typfields -> elab_deftyp_struct ctx deftyp.at tparams typfields
  | VariantTD typcases -> elab_deftyp_variant ctx deftyp.at id tparams typcases

(* Elaboration of plain type definitions *)

and elab_deftyp_plain (ctx : Ctx.t) (tparams : tparam list)
    (plaintyp : plaintyp) : Typdef.t * Il.deftyp =
  let typ_il = elab_plaintyp ctx plaintyp in
  let deftyp_il = Il.PlainT typ_il $ plaintyp.at in
  let td = Typdef.Defined (tparams, deftyp_il) in
  (td, deftyp_il)

(* Elaboration of struct type definitions *)

and elab_typfield (ctx : Ctx.t) (typfield : typfield) : Il.typfield =
  let atom, plaintyp, _hints = typfield in
  let typ_il = elab_plaintyp ctx plaintyp in
  (atom, typ_il)

and elab_deftyp_struct (ctx : Ctx.t) (at : region) (tparams : tparam list)
    (typfields : typfield list) : Typdef.t * Il.deftyp =
  let typfields_il = List.map (elab_typfield ctx) typfields in
  let deftyp_il = Il.StructT typfields_il $ at in
  let td = Typdef.Defined (tparams, deftyp_il) in
  (td, deftyp_il)

(* Elaboration of variant type definitions *)

and elab_typcase_plain (ctx : Ctx.t) (typ_il : Il.typ) : Il.typcase list =
  let typ_il = Types.expand_typ ctx.tdenv typ_il in
  match typ_il.it with
  | Il.VarT { synid = tid; targs } -> (
      match Ctx.find_typdef ctx tid with
      | Typdef.Defined (tparams, deftyp) -> (
          match deftyp.it with
          | Il.PlainT typ_il ->
              let theta = List.combine tparams targs |> TIdMap.of_list in
              let typ_il = Envs.Il.Typ.subst_typ theta typ_il in
              elab_typcase_plain ctx typ_il
          | Il.VariantT typcases_il ->
              let theta = List.combine tparams targs |> TIdMap.of_list in
              List.map (Envs.Il.Typ.subst_typcase theta) typcases_il
          | _ ->
              let related =
                match Ctx.typdef_prior_at ctx tid with
                | Some prior_at -> [ (prior_at, "originally defined here") ]
                | None -> []
              in
              error typ_il.at "cannot extend a non-variant type"
                ~code:Extend_non_variant_struct ~related
                ~detail:
                  "A case-line `| T` extends the surrounding variant with the \
                   cases of `T`. The type named here has a body that is not a \
                   variant, so it has no cases to contribute.")
      | _ ->
          let related =
            match Ctx.typdef_prior_at ctx tid with
            | Some prior_at -> [ (prior_at, "originally declared here") ]
            | None -> []
          in
          error typ_il.at "cannot extend an incomplete type"
            ~code:Extend_incomplete ~related
            ~detail:
              "A case-line `| T` extends the surrounding variant with the \
               cases of `T`. The type named here was declared with `syntax T;` \
               but has no body yet, so there are no cases to contribute.")
  | _ ->
      error typ_il.at "cannot extend a non-variant type"
        ~code:Extend_non_variant_primitive
        ~detail:
          "A case-line `| T` extends the surrounding variant with the cases of \
           `T`. The expression here is a primitive type, not a named variant, \
           so there are no cases to contribute."

and elab_typcase (ctx : Ctx.t) (typorigin_il : Il.typorigin) (typcase : typcase)
    : Il.typcase list =
  let typ, hints = typcase in
  match typ with
  | PlainT plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      elab_typcase_plain ctx typ_il
  | NotationT nottyp ->
      let nottyp_il = elab_nottyp ctx (NotationT nottyp) in
      let hints_il = elab_hints ctx hints in
      [ { Il.notation = nottyp_il; origin = typorigin_il; hints = hints_il } ]

and elab_deftyp_variant (ctx : Ctx.t) (at : region) (id : id)
    (tparams : tparam list) (typcases : typcase list) : Typdef.t * Il.deftyp =
  let typorigin_il =
    let targs_il =
      List.map
        (fun tparam -> Il.VarT { synid = tparam; targs = [] } $ tparam.at)
        tparams
    in
    { Il.synid = id; targs = targs_il } $ id.at
  in
  let typcases_il = List.concat_map (elab_typcase ctx typorigin_il) typcases in
  let mixops_with_at =
    typcases_il
    |> List.map (fun typcase ->
           (Il.Mixfix.to_mixop typcase.Il.notation.it, typcase.Il.notation.at))
  in
  let mixop_groups =
    groupby (fun (m1, _) (m2, _) -> Il.Mixfix.eq_mixop m1 m2) mixops_with_at
  in
  let mixop_duplicates =
    List.filter (fun mixop_group -> List.length mixop_group > 1) mixop_groups
  in
  let related =
    mixop_duplicates
    |> List.concat_map (fun group ->
           let mixop, _ = List.hd group in
           let label =
             Printf.sprintf "case with shape %s" (Il.Mixfix.to_string mixop)
           in
           List.map (fun (_, at) -> (at, label)) group)
  in
  check
    (List.length mixop_duplicates = 0)
    at
    ("variant cases are ambiguous: "
    ^ String.concat ", "
        (List.map
           (fun mixop_group ->
             let mixop, _ = List.hd mixop_group in
             Il.Mixfix.to_string mixop)
           mixop_duplicates))
    ~code:Variant_mixop_collision ~related
    ~detail:
      "Variant cases must differ in their literal tokens or argument \
       positions; differences in argument types do not register. The cases \
       shown here share the same sequence, so the elaborator cannot pick \
       between them.";
  let deftyp_il = Il.VariantT typcases_il $ at in
  let td = Typdef.Defined (tparams, deftyp_il) in
  (td, deftyp_il)

(* Expressions *)

(* Inference of expression type *)

and fail_infer (at : region) (construct : string) =
  fail at ("cannot infer type of " ^ construct)

and infer_exp (ctx : Ctx.t) (exp : exp) : (Ctx.t * Il.exp * Il.typ) attempt =
  let* ctx, exp_il, typ_il = infer_exp' ctx exp.at exp.it in
  let exp_il = exp_il $$ (exp.at, typ_il) in
  let typ_il = typ_il $ exp.at in
  Ok (ctx, exp_il, typ_il)

and infer_exp' (ctx : Ctx.t) (at : region) (exp : exp') :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  match exp with
  | BoolE b -> infer_bool_exp ctx b
  | NumE (_, num) -> infer_num_exp ctx num
  | TextE text -> infer_text_exp ctx text
  | VarE id -> infer_var_exp ctx id
  | UnE (unop, exp) -> infer_unop_exp ctx at unop exp
  | BinE (exp_l, binop, exp_r) -> infer_binop_exp ctx at binop exp_l exp_r
  | CmpE (exp_l, cmpop, exp_r) -> infer_cmpop_exp ctx at cmpop exp_l exp_r
  | ArithE exp -> infer_arith_exp ctx exp
  | EpsE -> fail_infer at "empty sequence"
  | ListE exps -> infer_list_exp ctx at exps
  | ConsE (exp_h, exp_t) -> infer_cons_exp ctx exp_h exp_t
  | CatE (exp_l, exp_r) -> infer_cat_exp ctx exp_l exp_r
  | IdxE (exp_b, exp_i) -> infer_idx_exp ctx exp_b exp_i
  | SliceE (exp_b, exp_l, exp_h) -> infer_slice_exp ctx exp_b exp_l exp_h
  | LenE exp -> infer_len_exp ctx exp
  | MemE (exp_e, exp_s) -> infer_mem_exp ctx exp_e exp_s
  | StrE _ -> fail_infer at "struct expression"
  | DotE (exp, atom) -> infer_dot_exp ctx exp atom
  | UpdE (exp_b, path, exp_f) -> infer_upd_exp ctx exp_b path exp_f
  | ParenE exp -> infer_paren_exp ctx exp
  | TupleE exps -> infer_tuple_exp ctx exps
  | CallE (id, targs, args) -> infer_call_exp ctx at id targs args
  | IterE (exp, iter) -> infer_iter_exp ctx exp iter
  | SubE (exp, plaintyp) -> infer_sub_exp ctx exp plaintyp
  | AtomE _ -> fail_infer at "atom"
  | SeqE _ -> fail_infer at "sequence expression"
  | InfixE _ -> fail_infer at "infix expression"
  | BrackE _ -> fail_infer at "bracket expression"
  | HoleE _ ->
      error at "misplaced hole" ~code:Hole_outside_hint
        ~detail:
          "A `%`, `%N`, `%%`, or `!%` marks an argument slot inside a \
           `hint(...)` expression, like `hint(input %0 %1)`. Outside a hint, \
           it has no meaning."
  | FuseE _ ->
      error at "misplaced token concatenation" ~code:Fuse_outside_hint
        ~detail:
          "The `#` operator joins two fragments without a space inside a \
           `hint(...)` expression's rendered output, like `hint(prose \
           %0#suffix)`. Outside a hint, it has no meaning."
  | UnparenE _ ->
      error at "misplaced unparenthesize" ~code:Unparen_outside_hint
        ~detail:
          "The `##` operator strips enclosing parentheses from its operand \
           when a `hint(...)` expression is rendered, giving finer control \
           over the rendered form. Outside a hint, it has no meaning."
  | LatexE _ ->
      error at "misplaced LaTeX literal" ~code:Latex_outside_hint
        ~detail:
          "A `%latex(\"...\")` literal embeds raw LaTeX source inside a \
           `hint(...)` expression, for use by a LaTeX rendering backend. \
           Outside a hint, it has no meaning."

and infer_exps (ctx : Ctx.t) (exps : exp list) :
    (Ctx.t * Il.exp list * Il.typ list) attempt =
  match exps with
  | [] -> Ok (ctx, [], [])
  | exp :: exps ->
      let* ctx, exp_il, typ_il = infer_exp ctx exp in
      let* ctx, exps_il, typs_il = infer_exps ctx exps in
      Ok (ctx, exp_il :: exps_il, typ_il :: typs_il)

(* Inference of boolean expressions *)

and infer_bool_exp (ctx : Ctx.t) (b : bool) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let exp_il = Il.BoolE b in
  Ok (ctx, exp_il, Il.BoolT)

(* Inference of number expressions *)

and infer_num_exp (ctx : Ctx.t) (num : Num.t) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let exp_il = Il.NumE num in
  Ok (ctx, exp_il, Il.NumT (Num.to_typ num))

(* Inference of text expressions *)

and infer_text_exp (ctx : Ctx.t) (text : string) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let exp_il = Il.TextE text in
  Ok (ctx, exp_il, Il.TextT)

(* Inference of variable expressions *)

and infer_var_exp (ctx : Ctx.t) (id : id) : (Ctx.t * Il.exp' * Il.typ') attempt
    =
  let tid = Var.strip_var_suffix id in
  let meta_opt = Ctx.find_metavar_opt ctx tid in
  match meta_opt with
  | Some typ_il ->
      let exp_il = Il.VarE id in
      Ok (ctx, exp_il, typ_il.it)
  | None -> fail_infer id.at "variable"

(* Inference of unary expressions *)

and infer_unop (ctx : Ctx.t) (at : region) (unop : unop) (typ_il : Il.typ)
    (exp_il : Il.exp) : (Il.optyp * Il.exp * Il.typ') attempt =
  let unop_candidates =
    match unop with
    | #Bool.unop -> [ (`BoolT, Il.BoolT, Il.BoolT) ]
    | #Num.unop ->
        [
          (`NatT, Il.NumT `NatT, Il.NumT `NatT);
          (`IntT, Il.NumT `IntT, Il.NumT `IntT);
        ]
  in
  let fail =
    fail at
      (Format.asprintf "unary operator `%s` is not defined for operand type %s"
         (Il.Print.string_of_unop unop)
         (Il.Print.string_of_typ typ_il))
  in
  List.fold_left
    (fun unop_infer (optyp_il, typ_il_expect, typ_il_res_expect) ->
      match unop_infer with
      | Ok _ -> unop_infer
      | _ -> (
          let exp_il_attempt =
            cast_exp ctx (typ_il_expect $ typ_il.at) typ_il exp_il
          in
          match exp_il_attempt with
          | Ok exp_il -> Ok (optyp_il, exp_il, typ_il_res_expect)
          | _ -> fail))
    fail unop_candidates

and infer_unop_exp (ctx : Ctx.t) (at : region) (unop : unop) (exp : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il, typ_il = infer_exp ctx exp in
  let* optyp_il, exp_il, typ_il_expect = infer_unop ctx at unop typ_il exp_il in
  let exp_il = Il.UnE (unop, optyp_il, exp_il) in
  Ok (ctx, exp_il, typ_il_expect)

(* Inference of binary expressions *)

and infer_binop (ctx : Ctx.t) (at : region) (binop : binop) (typ_il_l : Il.typ)
    (exp_il_l : Il.exp) (typ_il_r : Il.typ) (exp_il_r : Il.exp) :
    (Il.optyp * Il.exp * Il.exp * Il.typ') attempt =
  let binop_candidates =
    match binop with
    | #Bool.binop -> [ (`BoolT, Il.BoolT, Il.BoolT, Il.BoolT) ]
    | #Num.binop ->
        [
          (`NatT, Il.NumT `NatT, Il.NumT `NatT, Il.NumT `NatT);
          (`IntT, Il.NumT `IntT, Il.NumT `IntT, Il.NumT `IntT);
        ]
  in
  let fail =
    fail at
      (Format.asprintf
         "binary operator `%s` is not defined for operand types %s and %s"
         (Il.Print.string_of_binop binop)
         (Il.Print.string_of_typ typ_il_l)
         (Il.Print.string_of_typ typ_il_r))
  in
  List.fold_left
    (fun binop_infer
         (optyp_il, typ_il_l_expect, typ_il_r_expect, typ_il_res_expect) ->
      match binop_infer with
      | Ok _ -> binop_infer
      | _ -> (
          let exp_il_l_attempt =
            cast_exp ctx (typ_il_l_expect $ typ_il_l.at) typ_il_l exp_il_l
          in
          let exp_il_r_attempt =
            cast_exp ctx (typ_il_r_expect $ typ_il_r.at) typ_il_r exp_il_r
          in
          match (exp_il_l_attempt, exp_il_r_attempt) with
          | Ok exp_il_l, Ok exp_il_r ->
              Ok (optyp_il, exp_il_l, exp_il_r, typ_il_res_expect)
          | _ -> fail))
    fail binop_candidates

and infer_binop_exp (ctx : Ctx.t) (at : region) (binop : binop) (exp_l : exp)
    (exp_r : exp) : (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il_l, typ_il_l = infer_exp ctx exp_l in
  let* ctx, exp_il_r, typ_il_r = infer_exp ctx exp_r in
  let* optyp_il, exp_il_l, exp_il_r, typ_il_expect =
    infer_binop ctx at binop typ_il_l exp_il_l typ_il_r exp_il_r
  in
  let exp_il = Il.BinE (binop, optyp_il, exp_il_l, exp_il_r) in
  Ok (ctx, exp_il, typ_il_expect)

(* Inference of comparison expressions *)

and infer_cmpop_exp_bool (ctx : Ctx.t) (cmpop : Bool.cmpop) (exp_l : exp)
    (exp_r : exp) : (Ctx.t * Il.exp' * Il.typ') attempt =
  choice
    [
      (fun () ->
        let* ctx, exp_il_r, typ_il_r = infer_exp ctx exp_r in
        let* ctx, exp_il_l = elab_exp ctx typ_il_r exp_l in
        let exp_il =
          Il.CmpE ((cmpop :> Il.cmpop), `BoolT, exp_il_l, exp_il_r)
        in
        Ok (ctx, exp_il, Il.BoolT));
      (fun () ->
        let* ctx, exp_il_l, typ_il_l = infer_exp ctx exp_l in
        let* ctx, exp_il_r = elab_exp ctx typ_il_l exp_r in
        let exp_il =
          Il.CmpE ((cmpop :> Il.cmpop), `BoolT, exp_il_l, exp_il_r)
        in
        Ok (ctx, exp_il, Il.BoolT));
    ]

and infer_cmpop_num (ctx : Ctx.t) (at : region) (cmpop : Num.cmpop)
    (typ_il_l : Il.typ) (exp_il_l : Il.exp) (typ_il_r : Il.typ)
    (exp_il_r : Il.exp) : (Il.optyp * Il.exp * Il.exp) attempt =
  let cmpop_candidates =
    [
      (`NatT, Il.NumT `NatT, Il.NumT `NatT);
      (`IntT, Il.NumT `IntT, Il.NumT `IntT);
    ]
  in
  let fail =
    fail at
      (Format.asprintf
         "comparison operator `%s` is not defined for operand types %s and %s"
         (Il.Print.string_of_cmpop (cmpop :> Il.cmpop))
         (Il.Print.string_of_typ typ_il_l)
         (Il.Print.string_of_typ typ_il_r))
  in
  List.fold_left
    (fun cmpop_infer (optyp_il, typ_il_l_expect, typ_il_r_expect) ->
      match cmpop_infer with
      | Ok _ -> cmpop_infer
      | _ -> (
          let exp_il_l_attempt =
            cast_exp ctx (typ_il_l_expect $ typ_il_l.at) typ_il_l exp_il_l
          in
          let exp_il_r_attempt =
            cast_exp ctx (typ_il_r_expect $ typ_il_r.at) typ_il_r exp_il_r
          in
          match (exp_il_l_attempt, exp_il_r_attempt) with
          | Ok exp_il_l, Ok exp_il_r -> Ok (optyp_il, exp_il_l, exp_il_r)
          | _ -> fail))
    fail cmpop_candidates

and infer_cmpop_exp_num (ctx : Ctx.t) (at : region) (cmpop : Num.cmpop)
    (exp_l : exp) (exp_r : exp) : (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il_l, typ_il_l = infer_exp ctx exp_l in
  let* ctx, exp_il_r, typ_il_r = infer_exp ctx exp_r in
  let* optyp_il, exp_il_l, exp_il_r =
    infer_cmpop_num ctx at cmpop typ_il_l exp_il_l typ_il_r exp_il_r
  in
  let exp_il = Il.CmpE ((cmpop :> Il.cmpop), optyp_il, exp_il_l, exp_il_r) in
  Ok (ctx, exp_il, Il.BoolT)

and infer_cmpop_exp (ctx : Ctx.t) (at : region) (cmpop : cmpop) (exp_l : exp)
    (exp_r : exp) : (Ctx.t * Il.exp' * Il.typ') attempt =
  match cmpop with
  | #Bool.cmpop as cmpop -> infer_cmpop_exp_bool ctx cmpop exp_l exp_r
  | #Num.cmpop as cmpop -> infer_cmpop_exp_num ctx at cmpop exp_l exp_r

(* Inference of arithmetic expressions *)

and infer_arith_exp (ctx : Ctx.t) (exp : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  infer_exp' ctx exp.at exp.it

(* Inference of list expressions *)

and infer_list_exp (ctx : Ctx.t) (at : region) (exps : exp list) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  match exps with
  | [] -> fail_infer at "empty list"
  | exp :: exps ->
      let* ctx, exp_il, typ_il = infer_exp ctx exp in
      let* ctx, exps_il, typs_il = infer_exps ctx exps in
      if List.for_all (Types.equiv_typ ctx.tdenv typ_il) typs_il then
        let exp_il = Il.ListE (exp_il :: exps_il) in
        Ok (ctx, exp_il, Il.IterT { typ = typ_il; iter = Il.List })
      else fail_infer at "list with heterogeneous elements"

(* Inference of cons expressions *)

and infer_cons_exp (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il_h, typ_il_h = infer_exp ctx exp_h in
  let typ_il = Il.IterT { typ = typ_il_h; iter = Il.List } $ typ_il_h.at in
  let* ctx, exp_il_t = elab_exp ctx typ_il exp_t in
  let exp_il = Il.ConsE (exp_il_h, exp_il_t) in
  Ok (ctx, exp_il, typ_il.it)

(* Inference of concatenation expressions *)

and infer_cat_exp (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  choice
    [
      (fun () ->
        let* ctx, exp_il_l, typ_il_l = infer_exp ctx exp_l in
        let* typ_il_elem = as_list_typ ctx typ_il_l in
        let typ_il =
          Il.IterT { typ = typ_il_elem; iter = Il.List } $ typ_il_elem.at
        in
        let* ctx, exp_il_r = elab_exp ctx typ_il exp_r in
        let exp_il = Il.CatE (exp_il_l, exp_il_r) in
        Ok (ctx, exp_il, typ_il.it));
      (fun () ->
        let* ctx, exp_il_l = elab_exp ctx (Il.TextT $ exp_l.at) exp_l in
        let* ctx, exp_il_r = elab_exp ctx (Il.TextT $ exp_r.at) exp_r in
        let exp_il = Il.CatE (exp_il_l, exp_il_r) in
        Ok (ctx, exp_il, Il.TextT));
    ]

(* Inference of index expressions *)

and infer_idx_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  choice
    [
      (fun () ->
        let* ctx, exp_il_b, typ_il_b = infer_exp ctx exp_b in
        let* typ_il_elem = as_list_typ ctx typ_il_b in
        let* ctx, exp_il_i = elab_exp ctx (Il.NumT `NatT $ exp_i.at) exp_i in
        let exp_il = Il.IdxE (exp_il_b, exp_il_i) in
        Ok (ctx, exp_il, typ_il_elem.it));
      (fun () ->
        let* ctx, exp_il_b = elab_exp ctx (Il.TextT $ exp_b.at) exp_b in
        let* ctx, exp_il_i = elab_exp ctx (Il.NumT `NatT $ exp_i.at) exp_i in
        let exp_il = Il.IdxE (exp_il_b, exp_il_i) in
        Ok (ctx, exp_il, Il.TextT));
    ]

(* Inference of slice expressions *)

and infer_slice_exp (ctx : Ctx.t) (exp_b : exp) (exp_l : exp) (exp_h : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  choice
    [
      (fun () ->
        let* ctx, exp_il_b, typ_il_b = infer_exp ctx exp_b in
        let* _ = as_list_typ ctx typ_il_b in
        let* ctx, exp_il_l = elab_exp ctx (Il.NumT `NatT $ exp_l.at) exp_l in
        let* ctx, exp_il_h = elab_exp ctx (Il.NumT `NatT $ exp_h.at) exp_h in
        let exp_il = Il.SliceE (exp_il_b, exp_il_l, exp_il_h) in
        Ok (ctx, exp_il, typ_il_b.it));
      (fun () ->
        let* ctx, exp_il_b = elab_exp ctx (Il.TextT $ exp_b.at) exp_b in
        let* ctx, exp_il_l = elab_exp ctx (Il.NumT `NatT $ exp_l.at) exp_l in
        let* ctx, exp_il_h = elab_exp ctx (Il.NumT `NatT $ exp_h.at) exp_h in
        let exp_il = Il.SliceE (exp_il_b, exp_il_l, exp_il_h) in
        Ok (ctx, exp_il, Il.TextT));
    ]

(* Inference of member expressions *)

and infer_mem_exp (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  choice
    [
      (fun () ->
        let* ctx, exp_il_e, typ_il_e = infer_exp ctx exp_e in
        let* ctx, exp_il_s =
          elab_exp ctx
            (Il.IterT { typ = typ_il_e; iter = Il.List } $ typ_il_e.at)
            exp_s
        in
        let exp_il = Il.MemE (exp_il_e, exp_il_s) in
        Ok (ctx, exp_il, Il.BoolT));
      (fun () ->
        let* ctx, exp_il_s, typ_il_s = infer_exp ctx exp_s in
        let* typ_il_elem = as_list_typ ctx typ_il_s in
        let* ctx, exp_il_e = elab_exp ctx typ_il_elem exp_e in
        let exp_il = Il.MemE (exp_il_e, exp_il_s) in
        Ok (ctx, exp_il, Il.BoolT));
    ]

(* Inference of dot expressions *)

and infer_dot_exp (ctx : Ctx.t) (exp : exp) (atom : atom) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il, typ_il = infer_exp ctx exp in
  let* typfields_il = as_struct_typ ctx typ_il in
  let* typ_il_field =
    List.find_opt (fun (atom_t, _) -> atom.it = atom_t.it) typfields_il
    |> fun typfield_opt ->
    match typfield_opt with
    | Some (_, typ_il) -> Ok typ_il
    | None -> fail exp.at "cannot infer type of field"
  in
  let exp_il = Il.DotE (exp_il, atom) in
  Ok (ctx, exp_il, typ_il_field.it)

(* Inference of update expressions *)

and infer_upd_exp (ctx : Ctx.t) (exp_b : exp) (path : path) (exp_f : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il_b, typ_il_b = infer_exp ctx exp_b in
  let* ctx, path_il, typ_il_f = elab_path ctx typ_il_b path in
  let* ctx, exp_il_f = elab_exp ctx typ_il_f exp_f in
  let exp_il = Il.UpdE (exp_il_b, path_il, exp_il_f) in
  Ok (ctx, exp_il, typ_il_b.it)

(* Inference of length expressions *)

and infer_len_exp (ctx : Ctx.t) (exp : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  choice
    [
      (fun () ->
        let* ctx, exp_il, typ_il = infer_exp ctx exp in
        let* _ = as_list_typ ctx typ_il in
        let exp_il = Il.LenE exp_il in
        Ok (ctx, exp_il, Il.NumT `NatT));
      (fun () ->
        let* ctx, exp_il = elab_exp ctx (Il.TextT $ exp.at) exp in
        let exp_il = Il.LenE exp_il in
        Ok (ctx, exp_il, Il.NumT `NatT));
    ]

(* Inference of parenthesized expressions *)

and infer_paren_exp (ctx : Ctx.t) (exp : exp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  infer_exp' ctx exp.at exp.it

(* Inference of tuple expressions *)

and infer_tuple_exp (ctx : Ctx.t) (exps : exp list) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exps_il, typs_il = infer_exps ctx exps in
  let exp_il = Il.TupleE exps_il in
  Ok (ctx, exp_il, Il.TupleT typs_il)

(* Inference of call expressions *)

and infer_call_exp (ctx : Ctx.t) (at : region) (id : id) (targs : targ list)
    (args : arg list) : (Ctx.t * Il.exp' * Il.typ') attempt =
  let tparams, params_il, typ_il = Ctx.find_dec_signature ctx id in
  check
    (List.length targs = List.length tparams)
    id.at "type arguments do not match" ~code:Call_targ_arity_mismatch;
  let targs_il = List.map (elab_plaintyp ctx) targs in
  let theta = List.combine tparams targs_il |> TIdMap.of_list in
  let params_il = Envs.Il.Typ.subst_params theta params_il in
  let typ_il = Envs.Il.Typ.subst_typ theta typ_il in
  let ctx, args_il = elab_args at ctx params_il args in
  let exp_il = Il.CallE (id, targs_il, args_il) in
  Ok (ctx, exp_il, typ_il.it)

(* Inference of iterated expressions *)

and infer_iter_exp (ctx : Ctx.t) (exp : exp) (iter : iter) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il, typ_il = infer_exp ctx exp in
  let iter_il = elab_iter iter in
  let exp_il = Il.IterE (exp_il, (iter_il, [])) in
  Ok (ctx, exp_il, Il.IterT { typ = typ_il; iter = iter_il })

(* Inference of subtype expressions *)

and infer_sub_exp (ctx : Ctx.t) (exp : exp) (plaintyp : plaintyp) :
    (Ctx.t * Il.exp' * Il.typ') attempt =
  let* ctx, exp_il, typ_il_exp = infer_exp ctx exp in
  let typ_il = elab_plaintyp ctx plaintyp in
  if
    Types.sub_typ ctx.tdenv typ_il_exp typ_il
    || Types.sub_typ ctx.tdenv typ_il typ_il_exp
  then
    let exp_il = Il.SubE (exp_il, typ_il) in
    Ok (ctx, exp_il, Il.BoolT)
  else
    fail exp.at
      (Format.asprintf "incomparable types %s and %s"
         (Il.Print.string_of_typ typ_il_exp)
         (Il.Print.string_of_typ typ_il))

(* Elaboration of expression type:

   - If an iterated type is expected,
      - first try elaborating the expression as a singleton iteration,
        but except wildcard, epsilon, and empty list expressions
      - then try usual elaboration
   - Otherwise, directly try usual elaboration *)

and elab_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  elab_exp' ctx typ_il_expect exp
  |> nest exp.at
       (Format.asprintf "elaboration of expression %s as type %s failed"
          (El.Unparse.string_of_exp exp)
          (Il.Print.string_of_typ typ_il_expect))

and elab_exp' (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  match as_iter_typ ctx typ_il_expect with
  | Ok (typ_il_expect_base, iter_expect) ->
      choice
        [
          (fun () ->
            match exp.it with
            | VarE id when id.it = "_" -> fail_silent
            | EpsE | ListE [] -> fail_silent
            | _ ->
                elab_exp_iter ctx typ_il_expect typ_il_expect_base iter_expect
                  exp);
          (fun () -> elab_exp_normal ctx typ_il_expect exp);
        ]
  | _ -> elab_exp_normal ctx typ_il_expect exp

and elab_exps (ctx : Ctx.t) (typs_il_expect : Il.typ list) (exps : exp list) :
    (Ctx.t * Il.exp list) attempt =
  match (typs_il_expect, exps) with
  | [], [] -> Ok (ctx, [])
  | [], _ -> fail no_region "more expressions than expected"
  | _, [] -> fail no_region "more expected types than expressions"
  | typ_il_expect :: typs_il_expect, exp :: exps ->
      let* ctx, exp_il = elab_exp ctx typ_il_expect exp in
      let* ctx, exps_il = elab_exps ctx typs_il_expect exps in
      Ok (ctx, exp_il :: exps_il)

(* Elaboration of expression as a singleton iteration *)

and elab_exp_iter (ctx : Ctx.t) (typ_il_expect : Il.typ)
    (typ_il_expect_base : Il.typ) (iter_il_expect : Il.iter) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  let* ctx, exp_il = elab_exp ctx typ_il_expect_base exp in
  match iter_il_expect with
  | Opt ->
      let exp_il = Il.OptE (Some exp_il) $$ (exp.at, typ_il_expect.it) in
      Ok (ctx, exp_il)
  | List ->
      let exp_il = Il.ListE [ exp_il ] $$ (exp.at, typ_il_expect.it) in
      Ok (ctx, exp_il)

(* Normal elaboration of expressions: a two-phase process,

   - if a type can be inferred without any contextual information,
     match the inferred type with the expected type
      - this may fail for some expressions that require contextual information,
        e.g., notation expressions or expression sequences
   - for such cases, try to elaborate the expression using the expected type *)

and fail_cast (at : region) (typ_il_a : Il.typ) (typ_il_b : Il.typ) =
  let msg =
    Format.asprintf "cannot cast %s to %s"
      (Il.Print.string_of_typ typ_il_a)
      (Il.Print.string_of_typ typ_il_b)
  in
  fail at msg

and cast_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (typ_il_infer : Il.typ)
    (exp_il : Il.exp) : Il.exp attempt =
  if Types.equiv_typ ctx.tdenv typ_il_expect typ_il_infer then Ok exp_il
  else if Types.sub_typ ctx.tdenv typ_il_infer typ_il_expect then
    let exp_il =
      Il.UpCastE (typ_il_expect, exp_il) $$ (exp_il.at, typ_il_expect.it)
    in
    Ok exp_il
  else fail_cast exp_il.at typ_il_infer typ_il_expect

and elab_exp_normal (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  let infer_attempt = infer_exp ctx exp in
  match infer_attempt with
  | Ok (ctx, exp_il, typ_il_infer) ->
      let* exp_il = cast_exp ctx typ_il_expect typ_il_infer exp_il in
      Ok (ctx, exp_il)
  | Error _ -> (
      match exp.it with
      | VarE id when id.it = "_" -> elab_exp_wildcard ctx exp.at typ_il_expect
      | _ -> (
          match typ_il_expect.it with
          | VarT { synid = tid; targs = targs_il } -> (
              let td = Ctx.find_typdef ctx tid in
              match td with
              | Param | Defining _ -> elab_exp_plain ctx typ_il_expect exp
              | Defined (tparams, deftyp_il) -> (
                  let theta = List.combine tparams targs_il |> TIdMap.of_list in
                  match deftyp_il.it with
                  | PlainT typ_il ->
                      let typ_il = Envs.Il.Typ.subst_typ theta typ_il in
                      elab_exp_normal ctx typ_il exp
                  | StructT typfields_il ->
                      let typfields_il =
                        List.map
                          (fun (atom, typ_il) ->
                            let typ_il = Envs.Il.Typ.subst_typ theta typ_il in
                            (atom, typ_il))
                          typfields_il
                      in
                      elab_exp_struct ctx typ_il_expect typfields_il exp
                  | VariantT typcases_il ->
                      let typcases_il =
                        List.map (Envs.Il.Typ.subst_typcase theta) typcases_il
                      in
                      elab_exp_variant ctx typ_il_expect typcases_il exp))
          | _ -> elab_exp_plain ctx typ_il_expect exp))

(* Elaboration of wildcard variable expressions *)

and elab_exp_wildcard (ctx : Ctx.t) (at : region) (typ_il_expect : Il.typ) :
    (Ctx.t * Il.exp) attempt =
  let { Il.varid = id_fresh; typ = typ_fresh; iters = iters_fresh } =
    Fresh.fresh_var_from_exp ~wildcard:true ctx.frees
      (Il.VarE ("_" $ at) $$ (at, typ_il_expect.it))
  in
  let ctx = Ctx.add_free ctx id_fresh in
  (* (TODO) Refactor here; this logic also exists in partialbind pass *)
  let exp_il =
    List.fold_left
      (fun exp iter ->
        let typ =
          let typ = exp.note $ exp.at in
          Il.IterT { typ; iter }
        in
        Il.IterE (exp, (iter, [])) $$ (exp.at, typ))
      (Il.VarE id_fresh $$ (id_fresh.at, typ_fresh.it))
      iters_fresh
  in
  Ok (ctx, exp_il)

(* Elaboration of plain expressions *)

and fail_elab_plain (at : region) (msg : string) =
  fail at ("cannot elaborate expression because " ^ msg)

and elab_exp_plain (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp : exp) :
    (Ctx.t * Il.exp) attempt =
  let* ctx, exp_il = elab_exp_plain' ctx exp.at typ_il_expect exp.it in
  let exp_il = exp_il $$ (exp.at, typ_il_expect.it) in
  Ok (ctx, exp_il)

and elab_exp_plain' (ctx : Ctx.t) (at : region) (typ_il_expect : Il.typ)
    (exp : exp') : (Ctx.t * Il.exp') attempt =
  match exp with
  | BoolE _ | NumE _ | TextE _ | VarE _ ->
      fail_elab_plain at
        (Format.asprintf "the type of %s should have been inferred"
           (El.Unparse.string_of_exp (exp $ at)))
  | EpsE -> elab_eps_exp ctx typ_il_expect
  | ListE exps -> elab_list_exp ctx typ_il_expect exps
  | ConsE (exp_h, exp_t) -> elab_cons_exp ctx typ_il_expect exp_h exp_t
  | CatE (exp_l, exp_r) -> elab_cat_exp ctx typ_il_expect exp_l exp_r
  | ParenE exp -> elab_paren_exp ctx typ_il_expect exp
  | TupleE exps -> elab_tuple_exp ctx typ_il_expect exps
  | IterE (exp, iter) -> elab_iter_exp ctx typ_il_expect exp iter
  | _ ->
      fail at
        (Format.asprintf "cannot elaborate expression %s as type %s"
           (El.Unparse.string_of_exp (exp $ at))
           (Il.Print.string_of_typ typ_il_expect))

(* Elaboration of episilon expressions *)

and elab_eps_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) :
    (Ctx.t * Il.exp') attempt =
  let* _typ_il_expect, iter_expect = as_iter_typ ctx typ_il_expect in
  let exp_il =
    match iter_expect with Opt -> Il.OptE None | List -> Il.ListE []
  in
  Ok (ctx, exp_il)

(* Elaboration of list expressions *)

and elab_list_exp_elementwise (ctx : Ctx.t) (typ_il_expect : Il.typ)
    (exps : exp list) : (Ctx.t * Il.exp list) attempt =
  match exps with
  | [] -> Ok (ctx, [])
  | exp :: exps ->
      let* ctx, exp_il = elab_exp ctx typ_il_expect exp in
      let* ctx, exps_il = elab_list_exp_elementwise ctx typ_il_expect exps in
      Ok (ctx, exp_il :: exps_il)

and elab_list_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exps : exp list) :
    (Ctx.t * Il.exp') attempt =
  let* typ_il_expect, iter_expect = as_iter_typ ctx typ_il_expect in
  match iter_expect with
  | Opt -> fail_elab_plain no_region "list expression with optional iteration"
  | List ->
      let* ctx, exps_il = elab_list_exp_elementwise ctx typ_il_expect exps in
      let exp_il = Il.ListE exps_il in
      Ok (ctx, exp_il)

(* Elaboration of cons expressions *)

and elab_cons_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp_h : exp)
    (exp_t : exp) : (Ctx.t * Il.exp') attempt =
  let* typ_il_expect, iter_expect = as_iter_typ ctx typ_il_expect in
  let* ctx, exp_il_h = elab_exp ctx typ_il_expect exp_h in
  let* ctx, exp_il_t =
    elab_exp ctx
      (Il.IterT { typ = typ_il_expect; iter = iter_expect } $ typ_il_expect.at)
      exp_t
  in
  let exp_il = Il.ConsE (exp_il_h, exp_il_t) in
  Ok (ctx, exp_il)

(* Elaboration of concatenation expressions *)

and elab_cat_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp_l : exp)
    (exp_r : exp) : (Ctx.t * Il.exp') attempt =
  choice
    [
      (fun () ->
        let* typ_il_expect, iter_il_expect = as_iter_typ ctx typ_il_expect in
        let typ_il_expect =
          Il.IterT { typ = typ_il_expect; iter = iter_il_expect }
          $ typ_il_expect.at
        in
        let* ctx, exp_il_l = elab_exp ctx typ_il_expect exp_l in
        let* ctx, exp_il_r = elab_exp ctx typ_il_expect exp_r in
        let exp_il = Il.CatE (exp_il_l, exp_il_r) in
        Ok (ctx, exp_il));
      (fun () ->
        let* ctx, exp_il_l = elab_exp ctx (Il.TextT $ exp_l.at) exp_l in
        let* ctx, exp_il_r = elab_exp ctx (Il.TextT $ exp_r.at) exp_r in
        let exp_il = Il.CatE (exp_il_l, exp_il_r) in
        Ok (ctx, exp_il));
    ]

(* Elaboration of tuple expressions *)

and elab_tuple_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exps : exp list) :
    (Ctx.t * Il.exp') attempt =
  let* typs_il_expect = as_tuple_typ ctx typ_il_expect in
  let* ctx, exps_il = elab_exps ctx typs_il_expect exps in
  let exp_il = Il.TupleE exps_il in
  Ok (ctx, exp_il)

(* Elaboration of parenthesized expressions *)

and elab_paren_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp : exp) :
    (Ctx.t * Il.exp') attempt =
  let* ctx, exp_il = elab_exp ctx typ_il_expect exp in
  Ok (ctx, exp_il.it)

(* Elaboration of iterated expressions *)

and elab_iter_exp (ctx : Ctx.t) (typ_il_expect : Il.typ) (exp : exp)
    (iter : iter) : (Ctx.t * Il.exp') attempt =
  let iter_il = elab_iter iter in
  let* typ_il_expect, iter_il_expect = as_iter_typ ctx typ_il_expect in
  if iter_il <> iter_il_expect then fail_elab_plain exp.at "iteration mismatch"
  else
    let* ctx, exp_il = elab_exp ctx typ_il_expect exp in
    let exp_il = Il.IterE (exp_il, (iter_il_expect, [])) in
    Ok (ctx, exp_il)

(* Elaboration of notation expressions *)

and fail_elab_not_inner (at : region) (msg : string) :
    (Ctx.t * Il.typ list * Il.exp list) attempt =
  fail at ("cannot elaborate notation expression because " ^ msg)

and elab_exp_not_inner (ctx : Ctx.t) (mixop : Mixop.t) (typs_il : Il.typ list)
    (exp : exp) : (Ctx.t * Il.typ list * Il.exp list) attempt =
  match (mixop, exp.it) with
  | _, ParenE exp -> elab_exp_not_inner ctx mixop typs_il exp
  | Arg, _ -> (
      match typs_il with
      | [] -> fail_elab_not_inner exp.at "too many arguments"
      | typ_il_h :: typs_il_t ->
          let* ctx, exp_il = elab_exp ctx typ_il_h exp in
          Ok (ctx, typs_il_t, [ exp_il ]))
  | Atom atom_t, AtomE atom_e when atom_t.it <> atom_e.it ->
      fail_elab_not_inner exp.at "atom does not match"
  | Atom _, AtomE _ -> Ok (ctx, typs_il, [])
  | Seq [], SeqE [] -> Ok (ctx, typs_il, [])
  | Seq (mixop_h :: mixops_t), SeqE (exp_h :: exps_t) ->
      let* ctx, typs_il, exps_il_h =
        elab_exp_not_inner ctx mixop_h typs_il exp_h
      in
      let* ctx, typs_il, exps_il_t =
        elab_exp_not_inner ctx (Mixop.Seq mixops_t) typs_il
          (SeqE exps_t $ exp.at)
      in
      let exps_il = exps_il_h @ exps_il_t in
      Ok (ctx, typs_il, exps_il)
  | Seq (_ :: _), SeqE [] -> fail_elab_not_inner exp.at "omitted sequence tail"
  | Seq [], SeqE (_ :: _) ->
      fail_elab_not_inner exp.at "expression is not empty"
  | Infix (_, atom_t, _), InfixE (_, atom_e, _) when atom_t.it <> atom_e.it ->
      fail_elab_not_inner exp.at "atoms do not match"
  | Infix (mixop_l, _, mixop_r), InfixE (exp_l, _, exp_r) ->
      let* ctx, typs_il, exps_il_l =
        elab_exp_not_inner ctx mixop_l typs_il exp_l
      in
      let* ctx, typs_il, exps_il_r =
        elab_exp_not_inner ctx mixop_r typs_il exp_r
      in
      let exps_il = exps_il_l @ exps_il_r in
      Ok (ctx, typs_il, exps_il)
  | Brack (atom_t_l, _, atom_t_r), BrackE (atom_e_l, _, atom_e_r)
    when atom_t_l.it <> atom_e_l.it || atom_t_r.it <> atom_e_r.it ->
      fail_elab_not_inner exp.at "atoms do not match"
  | Brack (_, mixop, _), BrackE (_, exp, _) ->
      elab_exp_not_inner ctx mixop typs_il exp
  | _ -> fail_elab_not_inner exp.at "expression does not match notation"

and fail_elab_not (at : region) (msg : string) : (Ctx.t * Il.notexp) attempt =
  fail at ("cannot elaborate notation expression because " ^ msg)

and elab_exp_not (ctx : Ctx.t) (nottyp_il : Il.nottyp) (exp : exp) :
    (Ctx.t * Il.notexp) attempt =
  let mixop, typs_il = Il.Mixfix.split nottyp_il.it in
  let mixop_el = Mixop.of_il mixop in
  let* ctx, typs_il, exps_il = elab_exp_not_inner ctx mixop_el typs_il exp in
  match typs_il with
  | [] ->
      let notexp_il = Il.Mixfix.fill mixop exps_il in
      Ok (ctx, notexp_il)
  | _ -> fail_elab_not exp.at "too few arguments"

(* Elaboration of struct expressions *)

and fail_elab_struct (at : region) (msg : string) :
    (Ctx.t * (Il.atom * Il.exp) list) attempt =
  fail at ("cannot elaborate struct expression because " ^ msg)

and elab_expfields (ctx : Ctx.t) (at : region)
    (typfields : (atom * Il.typ) list) (expfields : (atom * exp) list) :
    (Ctx.t * (Il.atom * Il.exp) list) attempt =
  match (typfields, expfields) with
  | [], [] -> Ok (ctx, [])
  | [], (atom_e, _) :: _ ->
      fail_elab_struct atom_e.at "expression has extra fields"
  | _ :: _, [] -> fail_elab_struct at "expression omitted struct fields"
  | (atom_t, _) :: _, (atom_e, _) :: _ when atom_t.it <> atom_e.it ->
      fail_elab_struct atom_e.at "atom does not match"
  | (atom_t, typ_il) :: typfields, (_, exp) :: expfields ->
      let* ctx, exp_il = elab_exp ctx typ_il exp in
      let* ctx, expfields_il = elab_expfields ctx at typfields expfields in
      Ok (ctx, (atom_t, exp_il) :: expfields_il)

and elab_exp_struct (ctx : Ctx.t) (typ_il_expect : Il.typ)
    (typfields_il : Il.typfield list) (exp : exp) : (Ctx.t * Il.exp) attempt =
  let* ctx, expfields_il = elab_exp_struct' ctx typfields_il exp in
  let exp_il = Il.StrE expfields_il $$ (exp.at, typ_il_expect.it) in
  Ok (ctx, exp_il)

and elab_exp_struct' (ctx : Ctx.t) (typfields_il : Il.typfield list) (exp : exp)
    : (Ctx.t * (Il.atom * Il.exp) list) attempt =
  match exp.it with
  | StrE expfields -> elab_expfields ctx exp.at typfields_il expfields
  | _ -> fail_elab_struct exp.at "expression is not a struct"

(* Elaboration of variant expressions

   This finds a single case that matches the expression,
   where it has the smallest possible type, according to the variant type subtyping rule

   Finding the smallest type is important because the interpreter needs to
   propagate the type information while evaluating expressions,
   and it has to perform runtime type checks of whether a value is a subtype of some particular type *)

and fail_elab_variant (at : region) (msg : string) : (Ctx.t * Il.exp) attempt =
  fail at ("cannot elaborate variant case because " ^ msg)

and elab_exp_variant (ctx : Ctx.t) (typ_il_expect : Il.typ)
    (typcases_il : Il.typcase list) (exp : exp) : (Ctx.t * Il.exp) attempt =
  let ctx, exps_il =
    List.fold_left
      (fun (ctx, exps_il) typcase_il ->
        let { Il.notation = nottyp_il; origin = typorigin_il; _ } =
          typcase_il
        in
        match elab_exp_not ctx nottyp_il exp with
        | Ok (ctx, notexp_il) ->
            let typ_il =
              let { Il.synid = id; targs = targs_il } = typorigin_il.it in
              Il.VarT { synid = id; targs = targs_il } $ typorigin_il.at
            in
            let exp_il = Il.CaseE notexp_il $$ (exp.at, typ_il.it) in
            let+ exp_il = cast_exp ctx typ_il_expect typ_il exp_il in
            (ctx, exps_il @ [ exp_il ])
        | Error _ -> (ctx, exps_il))
      (ctx, []) typcases_il
  in
  match exps_il with
  | [ exp_il ] -> Ok (ctx, exp_il)
  | [] -> fail_elab_variant exp.at "expression does not match any case"
  | _ -> fail_elab_variant exp.at "expression matches multiple cases"

(* Elaboration of paths *)

and elab_path (ctx : Ctx.t) (typ_il_expect : Il.typ) (path : path) :
    (Ctx.t * Il.path * Il.typ) attempt =
  let* ctx, path_il, typ_il = elab_path' ctx typ_il_expect path.it in
  let path_il = path_il $$ (path.at, typ_il) in
  let typ_il = typ_il $ path.at in
  Ok (ctx, path_il, typ_il)

and elab_path' (ctx : Ctx.t) (typ_il_expect : Il.typ) (path : path') :
    (Ctx.t * Il.path' * Il.typ') attempt =
  match path with
  | RootP -> elab_root_path ctx typ_il_expect
  | IdxP (path, exp) -> elab_idx_path ctx typ_il_expect path exp
  | SliceP (path, exp_l, exp_h) ->
      elab_slice_path ctx typ_il_expect path exp_l exp_h
  | DotP (path, atom) -> elab_dot_path ctx typ_il_expect path atom

(* Elaboration of root paths *)

and elab_root_path (ctx : Ctx.t) (typ_il_expect : Il.typ) :
    (Ctx.t * Il.path' * Il.typ') attempt =
  Ok (ctx, Il.RootP, typ_il_expect.it)

(* Elaboration of index paths *)

and elab_idx_path (ctx : Ctx.t) (typ_il_expect : Il.typ) (path : path)
    (exp : exp) : (Ctx.t * Il.path' * Il.typ') attempt =
  choice
    [
      (fun () ->
        let* ctx, path_il, typ_il = elab_path ctx typ_il_expect path in
        let* ctx, exp_il = elab_exp ctx (Il.NumT `NatT $ exp.at) exp in
        let path_il = Il.IdxP (path_il, exp_il) in
        let* typ_il = as_list_typ ctx typ_il in
        Ok (ctx, path_il, typ_il.it));
      (fun () ->
        let* ctx, path_il, typ_il = elab_path ctx typ_il_expect path in
        let* ctx, exp_il = elab_exp ctx (Il.NumT `NatT $ exp.at) exp in
        let path_il = Il.IdxP (path_il, exp_il) in
        let* _ = as_text_typ ctx typ_il in
        Ok (ctx, path_il, typ_il.it));
    ]

(* Elaboration of slice paths *)

and elab_slice_path (ctx : Ctx.t) (typ_il_expect : Il.typ) (path : path)
    (exp_l : exp) (exp_h : exp) : (Ctx.t * Il.path' * Il.typ') attempt =
  choice
    [
      (fun () ->
        let* ctx, path_il, typ_il = elab_path ctx typ_il_expect path in
        let* ctx, exp_il_l = elab_exp ctx (Il.NumT `NatT $ exp_l.at) exp_l in
        let* ctx, exp_il_h = elab_exp ctx (Il.NumT `NatT $ exp_h.at) exp_h in
        let path_il = Il.SliceP (path_il, exp_il_l, exp_il_h) in
        let* _ = as_list_typ ctx typ_il in
        Ok (ctx, path_il, typ_il.it));
      (fun () ->
        let* ctx, path_il, typ_il = elab_path ctx typ_il_expect path in
        let* ctx, exp_il_l = elab_exp ctx (Il.NumT `NatT $ exp_l.at) exp_l in
        let* ctx, exp_il_h = elab_exp ctx (Il.NumT `NatT $ exp_h.at) exp_h in
        let path_il = Il.SliceP (path_il, exp_il_l, exp_il_h) in
        let* _ = as_text_typ ctx typ_il in
        Ok (ctx, path_il, typ_il.it));
    ]

(* Elaboration of dot paths *)

and elab_dot_path (ctx : Ctx.t) (typ_il_expect : Il.typ) (path : path)
    (atom : atom) : (Ctx.t * Il.path' * Il.typ') attempt =
  let* ctx, path_il, typ_il = elab_path ctx typ_il_expect path in
  let* typfields_il = as_struct_typ ctx typ_il in
  let* typ_il =
    List.find_opt (fun (atom_t, _) -> atom.it = atom_t.it) typfields_il
    |> fun typfield_opt ->
    match typfield_opt with
    | Some (_, typ_il) -> Ok typ_il
    | None -> fail atom.at "cannot infer type of field"
  in
  let path_il = Il.DotP (path_il, atom) in
  Ok (ctx, path_il, typ_il.it)

(* Elaboration of parameters *)

and elab_param (ctx : Ctx.t) (param : param) : Il.param =
  match param.it with
  | ExpP plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      Il.ExpP typ_il $ param.at
  | DefP (id, tparams, params, plaintyp) ->
      check
        (List.map it tparams |> distinct ( = ))
        id.at "type parameters are not distinct"
        ~code:Funparam_tparam_not_distinct;
      let ctx_local = ctx in
      let ctx_local = Ctx.add_tparams ctx_local tparams in
      let params_il = List.map (elab_param ctx_local) params in
      let typ_il = elab_plaintyp ctx_local plaintyp in
      Il.DefP { defid = id; tparams; params = params_il; typ = typ_il }
      $ param.at

(* Elaboration of arguments: either as definition, or part of a call expression

   Handling of function parameters differs based on whether it is intended to be a definition

    - If it is a definition, the function argument must matched the name of the function parameter,
      and it adds the function definition to the context
    - Otherwise, the function argument must match the type of the function parameter *)

and elab_arg ?(as_def = false) (ctx : Ctx.t) (param_il : Il.param) (arg : arg) :
    Ctx.t * Il.arg =
  match (param_il.it, arg.it) with
  | ExpP typ_il, ExpA exp ->
      let+ ctx, exp_il = elab_exp ctx typ_il exp in
      let arg_il = Il.ExpA exp_il $ arg.at in
      (ctx, arg_il)
  | ( Il.DefP
        {
          defid = id_p;
          tparams = tparams_il_p;
          params = params_il_p;
          typ = typ_il_p;
        },
      DefA id_a )
    when as_def ->
      check (id_p.it = id_a.it) arg.at
        (Format.asprintf
           "function argument does not match the declared function parameter %s"
           (Id.to_string id_p))
        ~code:Funarg_shape_mismatch_sig
        ~related:[ (id_p.at, "declared here") ]
        ~detail:
          "A function argument in a `def` clause must bind to the same name as \
           the declared function parameter in the `dec`. The clause body uses \
           that name to call the function.";
      let ctx =
        Ctx.add_defined_dec ctx id_p tparams_il_p params_il_p typ_il_p
      in
      let arg_il = Il.DefA id_a $ arg.at in
      (ctx, arg_il)
  | ( Il.DefP
        {
          defid = id_p;
          tparams = tparams_il_p;
          params = params_il_p;
          typ = typ_il_p;
        },
      DefA id_a ) ->
      let tparams_il_a, params_il_a, typ_il_a =
        Ctx.find_dec_signature ctx id_a
      in
      let related =
        match Ctx.dec_prior_at ctx id_a with
        | Some prior_at -> [ (prior_at, "passed function declared here") ]
        | None -> []
      in
      check
        (Types.equiv_functyp ctx.tdenv arg.at tparams_il_p params_il_p typ_il_p
           tparams_il_a params_il_a typ_il_a)
        arg.at
        (Format.asprintf
           "function argument does not match the declared function parameter %s"
           (Id.to_string id_p))
        ~code:Funarg_shape_mismatch_call ~related
        ~detail:
          "A function argument at a call site must have the same signature \
           (type parameters, parameter types, and return type) as the declared \
           function parameter. The function passed here was declared with a \
           different signature.";
      let arg_il = Il.DefA id_a $ arg.at in
      (ctx, arg_il)
  | ExpP _, DefA _ ->
      error arg.at
        "expected an expression argument, but got a function argument"
        ~code:Funarg_expected_exp_got_fun
  | DefP _, ExpA _ ->
      error arg.at
        "expected a function argument, but got an expression argument"
        ~code:Funarg_expected_fun_got_exp

and elab_args ?(as_def = false) (at : region) (ctx : Ctx.t)
    (params_il : Il.param list) (args : arg list) : Ctx.t * Il.arg list =
  check
    (List.length args = List.length params_il)
    at "arguments do not match" ~code:Call_arg_arity_mismatch;
  List.fold_left2
    (fun (ctx, args_il) param_il arg ->
      let ctx, arg_il = elab_arg ~as_def ctx param_il arg in
      (ctx, args_il @ [ arg_il ]))
    (ctx, []) params_il args

(* Elaboration of premises *)

and elab_prem (ctx : Ctx.t) (prem : prem) : Ctx.t * Il.prem option =
  let ctx, prem_il_opt = elab_prem' ctx prem.it in
  let prem_il_opt = Option.map (fun prem_il -> prem_il $ prem.at) prem_il_opt in
  (ctx, prem_il_opt)

and elab_prem' (ctx : Ctx.t) (prem : prem') : Ctx.t * Il.prem' option =
  let wrap_ctx prem = (ctx, prem) in
  let wrap_some (ctx, prem) = (ctx, Some prem) in
  let wrap_none ctx = (ctx, None) in
  match prem with
  | VarPr (id, plaintyp) -> elab_var_prem ctx id plaintyp |> wrap_none
  | RulePr (id, exp) -> elab_rule_prem ctx id exp |> wrap_some
  | RuleNotPr (id, exp) -> elab_rule_not_prem ctx id exp |> wrap_some
  | IfPr exp -> elab_if_prem ctx exp |> wrap_some
  | ElsePr -> elab_else_prem () |> wrap_ctx |> wrap_some
  | IterPr (prem, iter) -> elab_iter_prem ctx prem iter |> wrap_some
  | DebugPr exp -> elab_debug_prem ctx exp |> wrap_some

and elab_prem_with_bind (ctx : Ctx.t) (prem : prem) : Ctx.t * Il.prem list =
  let ctx, prem_il_opt = elab_prem ctx prem in
  match prem_il_opt with
  | Some prem_il ->
      let ctx, prem_il, sideconditions_il =
        Dataflow.Analysis.analyze_prem ctx prem_il
      in
      let prem_il = { prem_il with prov = Some (Il.Source prem) } in
      let sideconditions_il =
        List.map
          (fun p -> { p with prov = Some Il.Synthesized })
          sideconditions_il
      in
      let prems_il = prem_il :: sideconditions_il in
      (ctx, prems_il)
  | None -> (ctx, [])

and elab_prems_with_bind (ctx : Ctx.t) (prems : prem list) :
    Ctx.t * Il.prem list =
  List.fold_left
    (fun (ctx, prems_il_acc) prem ->
      let ctx, prems_il = elab_prem_with_bind ctx prem in
      (ctx, prems_il_acc @ prems_il))
    (ctx, []) prems

(* Elaboration of variable premises *)

and elab_var_prem (ctx : Ctx.t) (id : id) (plaintyp : plaintyp) : Ctx.t =
  check (valid_tid id) id.at "invalid meta-variable identifier"
    ~code:Var_prem_invalid_metavar;
  let related =
    match Ctx.typdef_prior_at ctx id with
    | Some prior_at -> [ (prior_at, "originally defined here") ]
    | None -> []
  in
  check
    (not (Ctx.bound_typdef ctx id))
    id.at "type already defined" ~code:Var_prem_type_redefined ~related;
  let typ_il = elab_plaintyp ctx plaintyp in
  Ctx.add_metavar ctx id typ_il

(* Elaboration of rule premises *)

and elab_rule_prem (ctx : Ctx.t) (id : id) (exp : exp) : Ctx.t * Il.prem' =
  let reltyp_il = Ctx.find_rel ctx id in
  let nottyp_il = Il.Mode.notation reltyp_il.it $ reltyp_il.at in
  let+ ctx, notexp_il = elab_exp_not ctx nottyp_il exp in
  let call = { Il.relid = id; notexp = notexp_il } in
  if Il.Mode.is_predicate reltyp_il.it then
    let prem_il = Il.RelAssertPr { call; expect = true } in
    (ctx, prem_il)
  else
    let prem_il = Il.RelPr call in
    (ctx, prem_il)

(* Elaboration of negated rule premises *)

and elab_rule_not_prem (ctx : Ctx.t) (id : id) (exp : exp) : Ctx.t * Il.prem' =
  let reltyp_il = Ctx.find_rel ctx id in
  let nottyp_il = Il.Mode.notation reltyp_il.it $ reltyp_il.at in
  let+ ctx, notexp_il = elab_exp_not ctx nottyp_il exp in
  check
    (Il.Mode.is_predicate reltyp_il.it)
    exp.at "negated rule premises do not take inputs"
    ~code:Negated_premise_takes_inputs
    ~detail:
      "A negated rule premise asserts that a relation does not hold for given \
       inputs. The relation must therefore have only input positions: outputs \
       would have no value to produce when the relation fails.";
  let call = { Il.relid = id; notexp = notexp_il } in
  let prem_il = Il.RelAssertPr { call; expect = false } in
  (ctx, prem_il)

(* Elaboration of if premises *)

and elab_if_prem (ctx : Ctx.t) (exp : exp) : Ctx.t * Il.prem' =
  let+ ctx, exp_il = elab_exp ctx (Il.BoolT $ exp.at) exp in
  let prem_il = Il.IfPr { cond = exp_il; role = Il.Condition } in
  (ctx, prem_il)

(* Elaboration of else premises *)

and elab_else_prem () : Il.prem' = Il.ElsePr

(* Elaboration of iterated premises *)

and elab_iter_prem (ctx : Ctx.t) (prem : prem) (iter : iter) : Ctx.t * Il.prem'
    =
  check
    (match prem.it with VarPr _ | ElsePr -> false | _ -> true)
    prem.at "only rule or if premises can be iterated"
    ~code:Iter_only_rule_or_if_premise
    ~detail:
      "Each iteration of `(prem)*` runs `prem` once. Variable declarations and \
       `otherwise` are declared once per rule, not per iteration, so iterating \
       them has no meaning.";
  let iter_il = elab_iter iter in
  let ctx, prem_il_opt = elab_prem ctx prem in
  let prem_il = Option.get prem_il_opt in
  let prem_il = Il.IterPr (prem_il, (iter_il, [])) in
  (ctx, prem_il)

(* Elaboration of debug premises *)

and elab_debug_prem (ctx : Ctx.t) (exp : exp) : Ctx.t * Il.prem' =
  let+ ctx, exp_il, _ = infer_exp ctx exp in
  let prem_il = Il.DebugPr exp_il in
  (ctx, prem_il)

(* Elaboration of hints *)

and elab_hint (ctx : Ctx.t) (hint : hint) : Il.hint =
  ignore ctx;
  { hintid = hint.hintid; hintexp = hint.hintexp }

and elab_hints (ctx : Ctx.t) (hints : hint list) : Il.hint list =
  List.map (elab_hint ctx) hints

(* Elaboration of definitions *)

let rec elab_def (ctx : Ctx.t) (def : def) : Ctx.t * Il.def option =
  let wrap_some (ctx, def) = (ctx, Some def) in
  let wrap_none ctx = (ctx, None) in
  let at = def.at in
  match def.it with
  | SynD syns -> elab_syn_def ctx syns |> wrap_none
  | TypD (id, tparams, deftyp, _hints) ->
      elab_typ_def ctx id tparams deftyp |> wrap_some
  | VarD (id, plaintyp, _hints) -> elab_var_def ctx id plaintyp |> wrap_none
  | RelD (id, nottyp, hints) -> elab_rel_def ctx at id nottyp hints |> wrap_some
  | RuleD (id_rel, id_rule, exp, prems) ->
      elab_rule_def ctx at id_rel id_rule exp prems |> wrap_none
  | BuiltinDecD (id, tparams, params, plaintyp, hints) ->
      elab_builtin_dec_def ctx at id tparams params plaintyp hints |> wrap_some
  | DecD (id, tparams, params, plaintyp, _hints) ->
      elab_dec_def ctx at id tparams params plaintyp |> wrap_some
  | DefD (id, tparams, args, exp, prems) ->
      elab_def_def ctx at id tparams args exp prems |> wrap_none
  | SepD -> ctx |> wrap_none

(* Elaboration of type declarations *)

and elab_syn_def (ctx : Ctx.t) (syns : (id * tparam list) list) : Ctx.t =
  List.fold_left
    (fun ctx (id, tparams) ->
      (* Invariant: EL parser only emits synid with empty tparams. *)
      assert (tparams = []);
      check (valid_tid id) id.at "invalid type identifier" ~code:Syn_invalid_id;
      let td = Typdef.Defining tparams in
      let ctx = Ctx.add_typdef ctx id td in
      if tparams = [] then
        let typ_il = Il.VarT { synid = id; targs = [] } $ id.at in
        Ctx.add_metavar ctx id typ_il
      else ctx)
    ctx syns

(* Elaboration of type definitions *)

and elab_typ_def (ctx : Ctx.t) (id : id) (tparams : tparam list)
    (deftyp : deftyp) : Ctx.t * Il.def =
  let td_opt = Ctx.find_typdef_opt ctx id in
  let ctx =
    match td_opt with
    | Some (Typdef.Defining tparams_defining) ->
        let related =
          match Ctx.typdef_prior_at ctx id with
          | Some prior_at -> [ (prior_at, "forward-declared here") ]
          | None -> []
        in
        let tparams = List.map it tparams in
        let tparams_defining = List.map it tparams_defining in
        check
          (List.length tparams = List.length tparams_defining
          && List.for_all2 ( = ) tparams tparams_defining)
          id.at "type parameters do not match" ~code:Typ_tparam_mismatch
          ~related
          ~detail:
            "A `syntax T<...> = ...` body must repeat the type parameters from \
             its forward declaration with the same count and the same names in \
             the same order.";
        ctx
    | None ->
        check (valid_tid id) id.at "invalid type identifier"
          ~code:Typ_invalid_id;
        let td = Typdef.Defining tparams in
        let ctx = Ctx.add_typdef ctx id td in
        if tparams = [] then
          let typ_il = Il.VarT { synid = id; targs = [] } $ id.at in
          Ctx.add_metavar ctx id typ_il
        else ctx
    | _ ->
        let related =
          match Ctx.typdef_prior_at ctx id with
          | Some prior_at -> [ (prior_at, "originally defined here") ]
          | None -> []
        in
        error id.at "type was already defined" ~code:Typ_fully_redefined
          ~related
  in
  check
    (List.for_all valid_tid tparams)
    id.at "invalid type parameter" ~code:Typ_invalid_tparam;
  let ctx_local = Ctx.add_tparams ctx tparams in
  let td, deftyp_il = elab_deftyp ctx_local id tparams deftyp in
  let def_il =
    Il.TypD { synid = id; tparams; deftyp = deftyp_il } $ deftyp.at
  in
  let ctx = Ctx.update_typdef ctx id td in
  (ctx, def_il)

(* Elaboration of variables *)

and elab_var_def (ctx : Ctx.t) (id : id) (plaintyp : plaintyp) : Ctx.t =
  check (valid_tid id) id.at "invalid meta-variable identifier";
  check (not (Ctx.bound_typdef ctx id)) id.at "type already defined";
  let typ_il = elab_plaintyp ctx plaintyp in
  Ctx.add_metavar ctx id typ_il

(* Elaboration of relations *)

and fetch_rel_input_dirs (at : region) (nottyp_il : Il.nottyp)
    (hints : hint list) : Il.Mode.dir list =
  let len = nottyp_il.it |> Il.Mixfix.args |> List.length in
  let all_inputs () = List.init len (fun _ -> Il.Mode.Input) in
  let dirs_of_indices indices =
    List.init len (fun i ->
        if List.mem i indices then Il.Mode.Input else Il.Mode.Output)
  in
  let hint_input =
    List.find_map
      (fun hint -> if hint.hintid.it = "input" then Some hint.hintexp else None)
      hints
  in
  match hint_input with
  | Some hintexp -> (
      match Hints.Input.parse hintexp with
      | Some [] ->
          error at "malformed input hint: at least one input should be provided"
            ~code:Relation_input_hint_empty
      | Some inputs when not (distinct ( = ) inputs) ->
          error at "malformed input hint: inputs should be distinct"
            ~code:Relation_input_hint_duplicate_index
      | Some inputs when List.for_all (fun input -> input < len) inputs ->
          dirs_of_indices inputs
      | Some _ | None ->
          warn at
            (Format.asprintf
               "malformed input hint: should be a sequence of indexed holes \
                %%N (N < %d)"
               len)
            ~code:Relation_input_hint_non_hole;
          all_inputs ())
  | None ->
      warn at "no input hint provided" ~code:Relation_no_input_hint;
      all_inputs ()

and elab_rel_def (ctx : Ctx.t) (at : region) (id : id) (nottyp : nottyp)
    (hints : hint list) : Ctx.t * Il.def =
  let nottyp_il = elab_nottyp ctx (NotationT nottyp) in
  let dirs = fetch_rel_input_dirs at nottyp_il hints in
  let reltyp_il = Il.Mode.of_dirs nottyp_il.it dirs $ nottyp_il.at in
  let ctx = Ctx.add_rel ctx id reltyp_il in
  let def_il = Il.RelD { relid = id; reltyp = reltyp_il; rules = [] } $ at in
  (ctx, def_il)

(* Elaboration of rules *)

and elab_rule_def (ctx : Ctx.t) (at : region) (id_rel : id) (id_rule : id)
    (exp : exp) (prems : prem list) : Ctx.t =
  let reltyp_il = Ctx.find_rel ctx id_rel in
  let nottyp_il = Il.Mode.notation reltyp_il.it $ reltyp_il.at in
  let ctx_local = { ctx with frees = IdSet.empty } in
  let ctx_local =
    let def = RuleD (id_rel, id_rule, exp, prems) $ at in
    El.Free.free_id_def def |> Ctx.add_frees ctx_local
  in
  let+ ctx_local, notexp_il = elab_exp_not ctx_local nottyp_il exp in
  let mixop, exps_il = Il.Mixfix.split notexp_il in
  let exps_il_input, exps_il_output = Il.Mode.partition reltyp_il.it exps_il in
  let ctx_local, exps_il_input, sideconditions_il =
    Dataflow.Analysis.analyze_exps_as_bind ctx_local exps_il_input
  in
  let ctx_local, prems_il = elab_prems_with_bind ctx_local prems in
  let prems_il = List.map as_guard sideconditions_il @ prems_il in
  let exps_il_output =
    Dataflow.Analysis.analyze_exps_as_bound ctx_local exps_il_output
  in
  let notexp_il =
    let exps_il =
      Il.Mode.interleave reltyp_il.it ~ins:exps_il_input ~outs:exps_il_output
    in
    Il.Mixfix.fill mixop exps_il
  in
  let rule =
    { Il.ruleid = id_rule; concl = notexp_il; prems = prems_il } $ at
  in
  Ctx.add_rule ctx id_rel rule

(* Elaboration of function declarations *)

and elab_builtin_dec_def (ctx : Ctx.t) (at : region) (id : id)
    (tparams : tparam list) (params : param list) (plaintyp : plaintyp)
    (hints : hint list) : Ctx.t * Il.def =
  check
    (List.map it tparams |> distinct ( = ))
    id.at "type parameters are not distinct"
    ~code:Builtin_dec_tparam_not_distinct;
  let ctx_local = ctx in
  let ctx_local = Ctx.add_tparams ctx_local tparams in
  let params_il = List.map (elab_param ctx_local) params in
  let typ_il = elab_plaintyp ctx_local plaintyp in
  let hints_il = elab_hints ctx_local hints in
  let ctx = Ctx.add_builtin_dec ctx id tparams params_il typ_il in
  let def_il =
    Il.BuiltinDecD
      {
        defid = id;
        tparams;
        params = params_il;
        typ = typ_il;
        hints = hints_il;
      }
    $ at
  in
  (ctx, def_il)

and elab_dec_def (ctx : Ctx.t) (at : region) (id : id) (tparams : tparam list)
    (params : param list) (plaintyp : plaintyp) : Ctx.t * Il.def =
  check
    (List.map it tparams |> distinct ( = ))
    id.at "type parameters are not distinct" ~code:Dec_tparam_not_distinct;
  let ctx_local = ctx in
  let ctx_local = Ctx.add_tparams ctx_local tparams in
  let params_il = List.map (elab_param ctx_local) params in
  let typ_il = elab_plaintyp ctx_local plaintyp in
  let def_il =
    Il.DecD
      { defid = id; tparams; params = params_il; typ = typ_il; clauses = [] }
    $ at
  in
  let ctx = Ctx.add_defined_dec ctx id tparams params_il typ_il in
  (ctx, def_il)

(* Elaboration of function definitions *)

and elab_def_input_with_bind (ctx : Ctx.t) (at : region)
    (params_il : Il.param list) (args : arg list) :
    Ctx.t * Il.arg list * Il.prem list =
  let ctx, args_il = elab_args ~as_def:true at ctx params_il args in
  let ctx, args_il, sideconditions_il =
    Dataflow.Analysis.analyze_args_as_bind ctx args_il
  in
  (ctx, args_il, sideconditions_il)

and elab_def_output_with_bind (ctx : Ctx.t) (typ_il : Il.typ) (exp : exp) :
    Ctx.t * Il.exp =
  let+ ctx, exp_il = elab_exp ctx typ_il exp in
  let exp_il = Dataflow.Analysis.analyze_exp_as_bound ctx exp_il in
  (ctx, exp_il)

and elab_def_def (ctx : Ctx.t) (at : region) (id : id) (tparams : tparam list)
    (args : arg list) (exp : exp) (prems : prem list) : Ctx.t =
  let tparams_expected, params_il, typ_il, _ = Ctx.find_defined_dec ctx id in
  let related =
    match Ctx.dec_prior_at ctx id with
    | Some prior_at -> [ (prior_at, "declared here") ]
    | None -> []
  in
  check
    (List.length tparams = List.length tparams_expected
    && List.for_all2 ( = ) (List.map it tparams) (List.map it tparams_expected)
    )
    id.at "type parameters do not match" ~code:Clause_tparam_mismatch ~related
    ~detail:
      "A `def $f<...> = ...` clause must repeat the type parameters from its \
       `dec` declaration with the same count and the same names in the same \
       order.";
  check
    (List.length params_il = List.length args)
    at "arguments do not match" ~code:Clause_arg_arity_mismatch;
  let ctx_local = { ctx with frees = IdSet.empty } in
  let ctx_local =
    let def = DefD (id, tparams, args, exp, prems) $ at in
    El.Free.free_id_def def |> Ctx.add_frees ctx_local
  in
  let ctx_local = Ctx.add_tparams ctx_local tparams in
  let ctx_local, args_il, sideconditions_il =
    elab_def_input_with_bind ctx_local at params_il args
  in
  let ctx_local, prems_il = elab_prems_with_bind ctx_local prems in
  let prems_il = List.map as_guard sideconditions_il @ prems_il in
  let _ctx_local, exp_il = elab_def_output_with_bind ctx_local typ_il exp in
  let clause_il = { Il.args = args_il; body = exp_il; prems = prems_il } $ at in
  Ctx.add_defined_clause ctx id clause_il

(* Elaboration of spec *)

(* Populate rules to their respective relations *)

let populate_rule (ctx : Ctx.t) (def_il : Il.def) : Il.def =
  match def_il.it with
  | Il.RelD { relid = id; reltyp; rules = [] } ->
      let rules_il = Ctx.find_rules ctx id in
      Il.RelD { relid = id; reltyp; rules = rules_il } $ def_il.at
  | Il.RelD _ ->
      (* unreachable: elab_rel_def constructs RelD only with []. *)
      assert false
  | _ -> def_il

let populate_rules (ctx : Ctx.t) (spec_il : Il.spec) : Il.spec =
  let spec_il = List.map (populate_rule ctx) spec_il in
  List.iter
    (fun def_il ->
      match def_il.it with
      | Il.RelD { relid = id; rules = []; _ } ->
          warn def_il.at
            (Format.asprintf "relation %s has no rules defined" id.it)
            ~code:Relation_missing_rules
      | _ -> ())
    spec_il;
  spec_il

(* Populate clauses to their respective function declarations *)

let populate_clause (ctx : Ctx.t) (def_il : Il.def) : Il.def =
  match def_il.it with
  | Il.DecD
      {
        defid = id;
        tparams = tparams_il;
        params = params_il;
        typ = typ_il;
        clauses = [];
      } ->
      let _, _, _, clauses_il = Ctx.find_defined_dec ctx id in
      Il.DecD
        {
          defid = id;
          tparams = tparams_il;
          params = params_il;
          typ = typ_il;
          clauses = clauses_il;
        }
      $ def_il.at
  | Il.DecD _ ->
      (* unreachable: elab_dec_def constructs DecD only with []. *)
      assert false
  | _ -> def_il

let populate_clauses (ctx : Ctx.t) (spec_il : Il.spec) : Il.spec =
  let spec_il = List.map (populate_clause ctx) spec_il in
  List.iter
    (fun def_il ->
      match def_il.it with
      | Il.DecD { defid = id; clauses = []; _ } ->
          warn def_il.at
            (Format.asprintf "dec $%s has no clauses defined" id.it)
            ~code:Dec_missing_clauses
      | _ -> ())
    spec_il;
  spec_il

(* Elaborate and collect failtraces *)

let elab_defs_with_errors (ctx : Ctx.t) (defs : def list) :
    Ctx.t * Il.def list * Diag.t list =
  List.fold_left
    (fun (ctx, defs_il, errors) def ->
      try
        let ctx, def_il_opt = elab_def ctx def in
        match def_il_opt with
        | Some def_il -> (ctx, defs_il @ [ def_il ], errors)
        | None -> (ctx, defs_il, errors)
      with Diagnostic.ElabError e -> (ctx, defs_il, e :: errors))
    (ctx, [], []) defs

(* Hint validation *)

let validate_hint (subject : Hints.Registry.subject) (hint : hint) : unit =
  let tag = hint.hintid.it in
  let at = hint.hintid.at in
  match Hints.Registry.lookup tag with
  | None -> warn at (Format.asprintf "unknown hint tag \"%s\"" tag)
  | Some entry -> (
      if not (List.mem subject entry.subjects) then
        warn at
          (Format.asprintf "hint \"%s\" cannot attach to %s, only to %s" tag
             (Hints.Registry.string_of_subject subject)
             (Hints.Registry.string_of_subjects entry.subjects))
      else
        match entry.kind with
        | Hints.Registry.Input -> (
            match Hints.Input.parse hint.hintexp with
            | Some _ -> ()
            | None ->
                warn at
                  (Format.asprintf
                     "hint \"%s\" payload malformed: expected a sequence of \
                      indexed holes %%N"
                     tag))
        | Hints.Registry.Alter ->
            let _ = Hints.Alter.parse hint.hintexp in
            ()
        | Hints.Registry.Precedence -> (
            match Hints.Precedence.parse hint.hintexp with
            | Some _ -> ()
            | None ->
                warn at
                  (Format.asprintf
                     "hint \"%s\" payload malformed: expected a single \
                      operator or keyword atom, like '&&' or 'AND'"
                     tag))
        | Hints.Registry.Fields -> (
            match Hints.Fields.parse hint.hintexp with
            | Some _ -> ()
            | None ->
                warn at
                  (Format.asprintf
                     "hint \"%s\" payload malformed: expected a sequence of \
                      text literals"
                     tag)))

let validate_hints_typfield (typfield : typfield) : unit =
  let _, _, hints = typfield in
  List.iter (validate_hint Hints.Registry.Typfield) hints

let validate_hints_typcase (typcase : typcase) : unit =
  let _, hints = typcase in
  List.iter (validate_hint Hints.Registry.Typcase) hints

let validate_hints_deftyp (deftyp : deftyp) : unit =
  match deftyp.it with
  | PlainTD _ -> ()
  | StructTD typfields -> List.iter validate_hints_typfield typfields
  | VariantTD typcases -> List.iter validate_hints_typcase typcases

let validate_hints_def (def : def) : unit =
  match def.it with
  | SynD _ -> ()
  | TypD (_, _, deftyp, _) -> validate_hints_deftyp deftyp
  | VarD (_, _, hints) -> List.iter (validate_hint Hints.Registry.Var) hints
  | RelD (_, _, hints) -> List.iter (validate_hint Hints.Registry.Rel) hints
  | RuleD _ -> ()
  | BuiltinDecD (_, _, _, _, hints) ->
      List.iter (validate_hint Hints.Registry.Func) hints
  | DecD (_, _, _, _, hints) ->
      List.iter (validate_hint Hints.Registry.Func) hints
  | DefD _ -> ()
  | SepD -> ()

let validate_hints_spec (spec : spec) : unit = List.iter validate_hints_def spec

let elab_spec (spec : spec) : Lang.Il.spec Diagnostic.result =
  try
    validate_hints_spec spec;
    let ctx = Ctx.init () in
    let ctx, spec_il, errors = elab_defs_with_errors ctx spec in
    let spec_il = spec_il |> populate_rules ctx |> populate_clauses ctx in
    if errors = [] then Ok spec_il else Error errors
  with Diagnostic.ElabError e -> Error [ e ]

type error = Diagnostic.error
type 'a result = 'a Diagnostic.result

let error_to_diagnostics = Diagnostic.to_diagnostics
