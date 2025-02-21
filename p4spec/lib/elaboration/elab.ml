open Xl
open El.Ast
open Dom
open Envs
open Attempt
open Util.Source

(* Checks *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let distinct (eq : 'a -> 'a -> bool) (xs : 'a list) : bool =
  let rec distinct' xs =
    match xs with
    | [] -> true
    | x :: xs -> if List.exists (eq x) xs then false else distinct' xs
  in
  distinct' xs

(* Todo *)

let todo (at : region) (func : string) (msg : string) =
  let msg =
    Format.asprintf "(TODO : %s) %s: %s\n" func (string_of_region at) msg
  in
  fail no_region msg

(* Parentheses handling *)

let rec unparen_plaintyp (plaintyp : plaintyp) : plaintyp =
  match plaintyp.it with
  | ParenT plaintyp -> unparen_plaintyp plaintyp
  | _ -> plaintyp

let rec unparen_exp (exp : exp) : exp =
  match exp.it with ParenE exp -> unparen_exp exp | _ -> exp

(* Identifiers *)

let valid_tid (id : id) = id.it = (Var.strip_var_suffix id).it

(* Iteration elaboration *)

let elab_iter (iter : iter) : Il.Ast.iter =
  match iter with Opt -> Il.Ast.Opt | List -> Il.Ast.List

(* Types *)

type kind =
  [ `Opaque
  | `Plain of plaintyp
  | `Struct of typfield list
  | `Variant of nottyp list ]

let rec kind_of_typ (ctx : Ctx.t) (plaintyp : plaintyp) : kind =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | VarT (tid, targs) -> (
      let td = Ctx.find_typdef ctx tid in
      match td with
      | Defined (tparams, typdef) -> (
          let theta = List.combine tparams targs |> Subst.Theta.of_list in
          match typdef with
          | `Plain plaintyp ->
              let plaintyp = Subst.subst_plaintyp theta plaintyp in
              `Plain plaintyp
          | `Struct typfields ->
              let typfields =
                List.map
                  (fun (atom, plaintyp, hints) ->
                    let plaintyp = Subst.subst_plaintyp theta plaintyp in
                    (atom, plaintyp, hints))
                  typfields
              in
              `Struct typfields
          | `Variant nottyps ->
              let nottyps = Subst.subst_nottyps theta nottyps in
              `Variant nottyps)
      | _ -> `Opaque)
  | _ -> `Plain plaintyp

(* Expansion of type aliases *)

and expand_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) : plaintyp =
  match plaintyp.it with
  | VarT (tid, _) -> (
      let td = Ctx.find_typdef ctx tid in
      match td with
      | Defined (_, typdef) -> (
          match typdef with
          | `Plain plaintyp -> expand_plaintyp ctx plaintyp
          | _ -> plaintyp)
      | _ -> plaintyp)
  | _ -> plaintyp

(* Type equivalence and subtyping *)

let rec equiv_typ (ctx : Ctx.t) (typ_a : typ) (typ_b : typ) : bool =
  match (typ_a, typ_b) with
  | PlainT plaintyp_a, PlainT plaintyp_b ->
      equiv_plaintyp ctx plaintyp_a plaintyp_b
  | NotationT nottyp_a, NotationT nottyp_b -> equiv_nottyp ctx nottyp_a nottyp_b
  | _ -> false

and equiv_plaintyp (ctx : Ctx.t) (plaintyp_a : plaintyp) (plaintyp_b : plaintyp)
    : bool =
  let plaintyp_a = expand_plaintyp ctx plaintyp_a in
  let plaintyp_b = expand_plaintyp ctx plaintyp_b in
  match (plaintyp_a.it, plaintyp_b.it) with
  | BoolT, BoolT -> true
  | NumT numtyp_a, NumT numtyp_b -> Num.equiv numtyp_a numtyp_b
  | TextT, TextT -> true
  | VarT (tid_a, targs_a), VarT (tid_b, targs_b) ->
      tid_a.it = tid_b.it
      && List.length targs_a = List.length targs_b
      && List.for_all2 (equiv_plaintyp ctx) targs_a targs_b
  | ParenT plaintyp_a, _ -> equiv_plaintyp ctx plaintyp_a plaintyp_b
  | _, ParenT plaintyp_b -> equiv_plaintyp ctx plaintyp_a plaintyp_b
  | TupleT plaintyps_a, TupleT plaintyps_b ->
      List.length plaintyps_a = List.length plaintyps_b
      && List.for_all2 (equiv_plaintyp ctx) plaintyps_a plaintyps_b
  | IterT (plaintyp_a, iter_a), IterT (plaintyp_b, iter_b) ->
      equiv_plaintyp ctx plaintyp_a plaintyp_b && iter_a = iter_b
  | _ -> false

and equiv_nottyp (ctx : Ctx.t) (nottyp_a : nottyp) (nottyp_b : nottyp) : bool =
  match (nottyp_a.it, nottyp_b.it) with
  | AtomT atom_a, AtomT atom_b -> atom_a.it = atom_b.it
  | SeqT typs_a, SeqT typs_b ->
      List.length typs_a = List.length typs_b
      && List.for_all2 (equiv_typ ctx) typs_a typs_b
  | InfixT (typ_l_a, atom_a, typ_r_a), InfixT (typ_l_b, atom_b, typ_r_b) ->
      equiv_typ ctx typ_l_a typ_l_b
      && atom_a.it = atom_b.it
      && equiv_typ ctx typ_r_a typ_r_b
  | BrackT (atom_l_a, typ_a, atom_r_a), BrackT (atom_l_b, typ_b, atom_r_b) ->
      atom_l_a.it = atom_l_b.it && equiv_typ ctx typ_a typ_b
      && atom_r_a.it = atom_r_b.it
  | _ -> false

let rec sub_plaintyp (ctx : Ctx.t) (plaintyp_a : plaintyp)
    (plaintyp_b : plaintyp) : bool =
  equiv_plaintyp ctx plaintyp_a plaintyp_b
  || sub_plaintyp' ctx plaintyp_a plaintyp_b

and sub_plaintyp' (ctx : Ctx.t) (plaintyp_a : plaintyp) (plaintyp_b : plaintyp)
    : bool =
  let plaintyp_a = expand_plaintyp ctx plaintyp_a in
  let plaintyp_b = expand_plaintyp ctx plaintyp_b in
  match (plaintyp_a.it, plaintyp_b.it) with
  | NumT numtyp_a, NumT numtyp_b -> Num.sub numtyp_a numtyp_b
  | VarT _, VarT _ -> (
      let kind_a = kind_of_typ ctx plaintyp_a in
      let kind_b = kind_of_typ ctx plaintyp_b in
      match (kind_a, kind_b) with
      | `Variant nottyps_a, `Variant nottyps_b ->
          List.for_all
            (fun nottyp_a -> List.exists (equiv_nottyp ctx nottyp_a) nottyps_b)
            nottyps_a
      | _ -> false)
  | ParenT plaintyp_a, _ -> sub_plaintyp ctx plaintyp_a plaintyp_b
  | _, ParenT plaintyp_b -> sub_plaintyp ctx plaintyp_a plaintyp_b
  | TupleT plaintyps_a, TupleT plaintyps_b ->
      List.length plaintyps_a = List.length plaintyps_b
      && List.for_all2 (sub_plaintyp ctx) plaintyps_a plaintyps_b
  | IterT (plaintyp_a, iter_a), IterT (plaintyp_b, iter_b) when iter_a = iter_b
    ->
      sub_plaintyp ctx plaintyp_a plaintyp_b
  | IterT (plaintyp_a, Opt), IterT (plaintyp_b, List) ->
      sub_plaintyp ctx plaintyp_a plaintyp_b
  | _, IterT (plaintyp_b, Opt) -> sub_plaintyp ctx plaintyp_a plaintyp_b
  | _, IterT (plaintyp_b, List) -> sub_plaintyp ctx plaintyp_a plaintyp_b
  | _ -> false

(* Elaboration of plain types *)

let rec elab_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) : Il.Ast.typ =
  let typ_il = elab_plaintyp' ctx plaintyp.it in
  typ_il $ plaintyp.at

and elab_plaintyp' (ctx : Ctx.t) (plaintyp : plaintyp') : Il.Ast.typ' =
  match plaintyp with
  | BoolT -> Il.Ast.BoolT
  | NumT numtyp -> Il.Ast.NumT numtyp
  | TextT -> Il.Ast.TextT
  | VarT (tid, targs) ->
      let td = Ctx.find_typdef ctx tid in
      let tparams = TypeDef.get_tparams td in
      check
        (List.length tparams = List.length targs)
        tid.at "type arguments do not match";
      let targs_il = List.map (elab_plaintyp ctx) targs in
      Il.Ast.VarT (tid, targs_il)
  | ParenT plaintyp -> elab_plaintyp' ctx plaintyp.it
  | TupleT plaintyps ->
      let typs_il = List.map (elab_plaintyp ctx) plaintyps in
      Il.Ast.TupleT typs_il
  | IterT (plaintyp, iter) ->
      let typ_il = elab_plaintyp ctx plaintyp in
      let iter_il = elab_iter iter in
      Il.Ast.IterT (typ_il, iter_il)

(* Elaboration of notation types *)

and elab_nottyp (ctx : Ctx.t) (typ : typ) : Il.Ast.nottyp =
  match typ with
  | PlainT plaintyp ->
      let mixop = [ []; [] ] in
      let typ_il = elab_plaintyp ctx plaintyp in
      (mixop, [ typ_il ]) $ plaintyp.at
  | NotationT nottyp -> (
      match nottyp.it with
      | AtomT atom ->
          let mixop = [ [ atom ] ] in
          let typs_il = [] in
          (mixop, typs_il) $ nottyp.at
      | SeqT [] ->
          let mixop = [ [] ] in
          let typs_il = [] in
          (mixop, typs_il) $ nottyp.at
      | SeqT (typ :: typs) ->
          let mixop_h, typs_il_h = elab_nottyp ctx typ |> it in
          let mixop_t, typs_il_t =
            elab_nottyp ctx (NotationT (SeqT typs $ nottyp.at)) |> it
          in
          let mixop = Mixop.merge mixop_h mixop_t in
          let typs_il = typs_il_h @ typs_il_t in
          (mixop, typs_il) $ nottyp.at
      | InfixT (typ_l, atom, typ_r) ->
          let mixop_l, typs_il_l = elab_nottyp ctx typ_l |> it in
          let mixop_r, typs_il_r = elab_nottyp ctx typ_r |> it in
          let mixop_l = Mixop.merge mixop_l [ [ atom ] ] in
          let mixop = Mixop.merge mixop_l mixop_r in
          let typs_il = typs_il_l @ typs_il_r in
          (mixop, typs_il) $ nottyp.at
      | BrackT (atom_l, typ, atom_r) ->
          let mixop, typs_il = elab_nottyp ctx typ |> it in
          let mixop_l = Mixop.merge [ [ atom_l ] ] mixop in
          let mixop = Mixop.merge mixop_l [ [ atom_r ] ] in
          (mixop, typs_il) $ nottyp.at)

(* Elaboration of definition types *)

and elab_deftyp (ctx : Ctx.t) (tparams : tparam list) (deftyp : deftyp) :
    TypeDef.t * Il.Ast.deftyp =
  match deftyp.it with
  | PlainTD plaintyp -> elab_typ_def_plain ctx tparams plaintyp
  | StructTD typfields -> elab_typ_def_struct ctx deftyp.at tparams typfields
  | VariantTD typcases -> elab_typ_def_variant ctx deftyp.at tparams typcases

(* Elaboration of plain type definitions *)

and elab_typ_def_plain (ctx : Ctx.t) (tparams : tparam list)
    (plaintyp : plaintyp) : TypeDef.t * Il.Ast.deftyp =
  let typ_il = elab_plaintyp ctx plaintyp in
  let deftyp_il = Il.Ast.PlainT typ_il $ plaintyp.at in
  let td = TypeDef.Defined (tparams, `Plain plaintyp) in
  (td, deftyp_il)

(* Elaboration of struct type definitions *)

and elab_typfield (ctx : Ctx.t) (typfield : typfield) : Il.Ast.typfield =
  let atom, plaintyp, _hints = typfield in
  let typ_il = elab_plaintyp ctx plaintyp in
  (atom, typ_il)

and elab_typ_def_struct (ctx : Ctx.t) (at : region) (tparams : tparam list)
    (typfields : typfield list) : TypeDef.t * Il.Ast.deftyp =
  let typfields_il = List.map (elab_typfield ctx) typfields in
  let deftyp_il = Il.Ast.StructT typfields_il $ at in
  let td = TypeDef.Defined (tparams, `Struct typfields) in
  (td, deftyp_il)

(* Elaboration of variant type definitions *)

and expand_typcase (ctx : Ctx.t) (typcase : typcase) : nottyp list =
  let typ, _hints = typcase in
  match typ with
  | PlainT plaintyp -> (
      let plaintyp = expand_plaintyp ctx plaintyp in
      let kind = kind_of_typ ctx plaintyp in
      match kind with
      | `Opaque -> error plaintyp.at "cannot extend an incomplete type"
      | `Variant nottyps -> nottyps
      | _ -> error plaintyp.at "cannot extend a non-variant type")
  | NotationT nottyp -> [ nottyp ]

and elab_typ_def_variant (ctx : Ctx.t) (at : region) (tparams : tparam list)
    (typcases : typcase list) : TypeDef.t * Il.Ast.deftyp =
  let nottyps = List.concat_map (expand_typcase ctx) typcases in
  let typcases_il =
    List.map (fun nottyp -> elab_nottyp ctx (NotationT nottyp)) nottyps
  in
  let mixops = typcases_il |> List.map it |> List.map fst in
  check (distinct Mixop.eq mixops) at "cases are ambiguous";
  let deftyp_il = Il.Ast.VariantT typcases_il $ at in
  let td = TypeDef.Defined (tparams, `Variant nottyps) in
  (td, deftyp_il)

(* Expressions *)

(* Inference of expression type *)

and infer_as_list (ctx : Ctx.t) (plaintyp : plaintyp) : plaintyp attempt =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | IterT (plaintyp, List) -> Ok plaintyp
  | _ -> fail plaintyp.at "cannot infer type as list"

and infer_as_struct (ctx : Ctx.t) (plaintyp : plaintyp) : typfield list attempt
    =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | VarT (tid, _) -> (
      let td_opt = Ctx.find_typdef_opt ctx tid in
      match td_opt with
      | Some (Defined (_, `Struct typfields)) -> Ok typfields
      | _ -> fail plaintyp.at "cannot infer type as struct")
  | _ -> fail plaintyp.at "cannot infer type as struct"

and fail_infer (at : region) (construct : string) =
  fail at ("cannot infer type of " ^ construct)

and infer_exp (ctx : Ctx.t) (exp : exp) : (Il.Ast.exp * plaintyp) attempt =
  let* exp_il, plaintyp = infer_exp' ctx exp.at exp.it in
  let typ_il = elab_plaintyp ctx (plaintyp $ exp.at) in
  Ok (exp_il $$ (exp.at, typ_il.it), plaintyp $ exp.at)

and infer_exp' (ctx : Ctx.t) (at : region) (exp : exp') :
    (Il.Ast.exp' * plaintyp') attempt =
  match exp with
  | BoolE b -> infer_bool_exp b
  | NumE (_, num) -> infer_num_exp num
  | TextE text -> infer_text_exp text
  | VarE (id, targs) -> infer_var_exp ctx id targs
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
  | CallE (id, targs, args) -> infer_call_exp ctx id targs args
  | IterE (exp, iter) -> infer_iter_exp ctx exp iter
  | TypE (exp, plaintyp) -> infer_typ_exp ctx exp plaintyp
  | AtomE _ -> fail_infer at "atom"
  | SeqE _ -> fail_infer at "sequence expression"
  | InfixE _ -> fail_infer at "infix expression"
  | BrackE _ -> fail_infer at "bracket expression"
  | HoleE _ -> error at "misplaced hole"
  | FuseE _ -> error at "misplaced token concatenation"
  | UnparenE _ -> error at "misplaced unparenthesize"
  | LatexE _ -> error at "misplaced LaTeX literal"

and infer_exps (ctx : Ctx.t) (exps : exp list) :
    (Il.Ast.exp list * plaintyp list) attempt =
  match exps with
  | [] -> Ok ([], [])
  | exp :: exps ->
      let* exp_il, plaintyp = infer_exp ctx exp in
      let* exps_il, plaintyps = infer_exps ctx exps in
      Ok (exp_il :: exps_il, plaintyp :: plaintyps)

(* Inference of boolean expressions *)

and infer_bool_exp (b : bool) : (Il.Ast.exp' * plaintyp') attempt =
  let exp_il = Il.Ast.BoolE b in
  let plaintyp = BoolT in
  Ok (exp_il, plaintyp)

(* Inference of number expressions *)

and infer_num_exp (num : Num.t) : (Il.Ast.exp' * plaintyp') attempt =
  let exp_il = Il.Ast.NumE num in
  let plaintyp = NumT (Num.to_typ num) in
  Ok (exp_il, plaintyp)

(* Inference of text expressions *)

and infer_text_exp (text : string) : (Il.Ast.exp' * plaintyp') attempt =
  let exp_il = Il.Ast.TextE text in
  let plaintyp = TextT in
  Ok (exp_il, plaintyp)

(* Inference of variable expressions *)

and infer_var_exp (ctx : Ctx.t) (id : id) (targs : targ list) :
    (Il.Ast.exp' * plaintyp') attempt =
  let tid = Var.strip_var_suffix id in
  let meta_opt = Ctx.find_metavar_opt ctx tid in
  match meta_opt with
  | Some _ when targs <> [] ->
      fail_infer id.at "meta-variable with type arguments is disallowed"
  | Some plaintyp ->
      let exp_il = Il.Ast.VarE id in
      Ok (exp_il, plaintyp.it)
  | None ->
      let exp_il = Il.Ast.VarE id in
      let plaintyp = VarT (tid, targs) in
      Ok (exp_il, plaintyp)

(* Inference of unary expressions *)

and infer_unop (ctx : Ctx.t) (at : region) (unop : unop) (plaintyp : plaintyp)
    (exp_il : Il.Ast.exp) : (Il.Ast.optyp * Il.Ast.exp * plaintyp') attempt =
  let optyp_il, plaintyps_expect =
    match unop with
    | #Bool.unop -> (`BoolT, [ (BoolT, BoolT) ])
    | #Num.unop ->
        (`NatT, [ (NumT `NatT, NumT `NatT); (NumT `IntT, NumT `IntT) ])
  in
  let fail =
    fail at
      ("unary operator `"
      ^ El.Print.string_of_unop unop
      ^ "` is not defined for operand type "
      ^ El.Print.string_of_plaintyp plaintyp)
  in
  List.fold_left
    (fun unop_infer (plaintyp_expect, plaintyp_res_expect) ->
      match unop_infer with
      | Ok _ -> unop_infer
      | _ -> (
          let exp_il_attempt =
            cast_exp ctx (plaintyp_expect $ plaintyp.at) plaintyp exp_il
          in
          match exp_il_attempt with
          | Ok exp_il -> Ok (optyp_il, exp_il, plaintyp_res_expect)
          | _ -> fail))
    fail plaintyps_expect

and infer_unop_exp (ctx : Ctx.t) (at : region) (unop : unop) (exp : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il, plaintyp = infer_exp ctx exp in
  let* optyp_il, exp_il, plaintyp_expect =
    infer_unop ctx at unop plaintyp exp_il
  in
  let exp_il = Il.Ast.UnE (unop, optyp_il, exp_il) in
  Ok (exp_il, plaintyp_expect)

(* Inference of binary expressions *)

and infer_binop (ctx : Ctx.t) (at : region) (binop : binop)
    (plaintyp_l : plaintyp) (exp_il_l : Il.Ast.exp) (plaintyp_r : plaintyp)
    (exp_il_r : Il.Ast.exp) :
    (Il.Ast.optyp * Il.Ast.exp * Il.Ast.exp * plaintyp') attempt =
  let optyp_il, plaintyps_expect =
    match binop with
    | #Bool.binop -> (`BoolT, [ (BoolT, BoolT, BoolT) ])
    | #Num.binop ->
        ( `NatT,
          [
            (NumT `NatT, NumT `NatT, NumT `NatT);
            (NumT `IntT, NumT `IntT, NumT `IntT);
          ] )
  in
  let fail =
    fail at
      ("binary operator `"
      ^ El.Print.string_of_binop binop
      ^ "` is not defined for operand types "
      ^ El.Print.string_of_plaintyp plaintyp_l
      ^ " and "
      ^ El.Print.string_of_plaintyp plaintyp_r)
  in
  List.fold_left
    (fun binop_infer (plaintyp_l_expect, plaintyp_r_expect, plaintyp_res_expect) ->
      match binop_infer with
      | Ok _ -> binop_infer
      | _ -> (
          let exp_il_l_attempt =
            cast_exp ctx (plaintyp_l_expect $ plaintyp_l.at) plaintyp_l exp_il_l
          in
          let exp_il_r_attempt =
            cast_exp ctx (plaintyp_r_expect $ plaintyp_r.at) plaintyp_r exp_il_r
          in
          match (exp_il_l_attempt, exp_il_r_attempt) with
          | Ok exp_il_l, Ok exp_il_r ->
              Ok (optyp_il, exp_il_l, exp_il_r, plaintyp_res_expect)
          | _ -> fail))
    fail plaintyps_expect

and infer_binop_exp (ctx : Ctx.t) (at : region) (binop : binop) (exp_l : exp)
    (exp_r : exp) : (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_l, plaintyp_l_infer = infer_exp ctx exp_l in
  let* exp_il_r, plaintyp_r_infer = infer_exp ctx exp_r in
  let* optyp_il, exp_il_l, exp_il_r, plaintyp_expect =
    infer_binop ctx at binop plaintyp_l_infer exp_il_l plaintyp_r_infer exp_il_r
  in
  let exp_il = Il.Ast.BinE (binop, optyp_il, exp_il_l, exp_il_r) in
  Ok (exp_il, plaintyp_expect)

(* Inference of comparison expressions *)

and infer_cmpop_exp_poly (ctx : Ctx.t) (at : region) (cmpop : Bool.cmpop)
    (exp_l : exp) (exp_r : exp) : (Il.Ast.exp' * plaintyp') attempt =
  choice at
    [
      (fun () ->
        let* exp_il_r, plaintyp_r = infer_exp ctx exp_r in
        let* exp_il_l = elab_exp ctx plaintyp_r exp_l in
        let exp_il =
          Il.Ast.CmpE ((cmpop :> Il.Ast.cmpop), `BoolT, exp_il_l, exp_il_r)
        in
        Ok (exp_il, BoolT));
      (fun () ->
        let* exp_il_l, plaintyp_l = infer_exp ctx exp_l in
        let* exp_il_r = elab_exp ctx plaintyp_l exp_r in
        let exp_il =
          Il.Ast.CmpE ((cmpop :> Il.Ast.cmpop), `BoolT, exp_il_l, exp_il_r)
        in
        Ok (exp_il, BoolT));
    ]

and infer_cmpop_exp (ctx : Ctx.t) (at : region) (cmpop : cmpop) (exp_l : exp)
    (exp_r : exp) : (Il.Ast.exp' * plaintyp') attempt =
  match cmpop with
  | #Bool.cmpop as cmpop -> infer_cmpop_exp_poly ctx at cmpop exp_l exp_r
  | #Num.cmpop -> todo at "infer_cmpop_exp" "Num.cmpop"

(* Inference of arithmetic expressions *)

and infer_arith_exp (ctx : Ctx.t) (exp : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  infer_exp' ctx exp.at exp.it

(* Inference of list expressions *)

and infer_list_exp (ctx : Ctx.t) (at : region) (exps : exp list) :
    (Il.Ast.exp' * plaintyp') attempt =
  match exps with
  | [] -> fail_infer at "empty list"
  | exp :: exps ->
      let* exp_il, plaintyp = infer_exp ctx exp in
      let* exps_il, plaintyps = infer_exps ctx exps in
      if List.for_all (equiv_plaintyp ctx plaintyp) plaintyps then
        let exp_il = Il.Ast.ListE (exp_il :: exps_il) in
        let plaintyp = IterT (plaintyp, List) in
        Ok (exp_il, plaintyp)
      else fail_infer at "list with heterogeneous elements"

(* Inference of cons expressions *)

and infer_cons_exp (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_h, plaintyp_h = infer_exp ctx exp_h in
  let plaintyp = IterT (plaintyp_h, List) in
  let* exp_il_t = elab_exp ctx (plaintyp $ plaintyp_h.at) exp_t in
  let exp_il = Il.Ast.ConsE (exp_il_h, exp_il_t) in
  Ok (exp_il, plaintyp)

(* Inference of concatenation expressions *)

and infer_cat_exp (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_l, plaintyp_l = infer_exp ctx exp_l in
  let* plaintyp = infer_as_list ctx plaintyp_l in
  let plaintyp = IterT (plaintyp, List) $ plaintyp.at in
  let* exp_il_r = elab_exp ctx plaintyp exp_r in
  let exp_il = Il.Ast.CatE (exp_il_l, exp_il_r) in
  Ok (exp_il, plaintyp.it)

(* Inference of index expressions *)

and infer_idx_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_b, plaintyp_b = infer_exp ctx exp_b in
  let* plaintyp = infer_as_list ctx plaintyp_b in
  let* exp_il_i = elab_exp ctx (NumT `NatT $ exp_i.at) exp_i in
  let exp_il = Il.Ast.IdxE (exp_il_b, exp_il_i) in
  Ok (exp_il, plaintyp.it)

(* Inference of slice expressions *)

and infer_slice_exp (ctx : Ctx.t) (exp_b : exp) (exp_l : exp) (exp_h : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_b, plaintyp_b = infer_exp ctx exp_b in
  let* plaintyp = infer_as_list ctx plaintyp_b in
  let* exp_il_l = elab_exp ctx (NumT `NatT $ exp_l.at) exp_l in
  let* exp_il_h = elab_exp ctx (NumT `NatT $ exp_h.at) exp_h in
  let exp_il = Il.Ast.SliceE (exp_il_b, exp_il_l, exp_il_h) in
  Ok (exp_il, plaintyp.it)

(* Inference of member expressions *)

and infer_mem_exp (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_e, plaintyp_e = infer_exp ctx exp_e in
  let* exp_il_s =
    elab_exp ctx (IterT (plaintyp_e, List) $ plaintyp_e.at) exp_s
  in
  let exp_il = Il.Ast.MemE (exp_il_e, exp_il_s) in
  let plaintyp = BoolT in
  Ok (exp_il, plaintyp)

(* Inference of dot expressions *)

and infer_dot_exp (ctx : Ctx.t) (exp : exp) (atom : atom) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il, plaintyp = infer_exp ctx exp in
  let* typfields = infer_as_struct ctx plaintyp in
  let* plaintyp =
    List.find_opt (fun (atom_t, _, _) -> atom.it = atom_t.it) typfields
    |> fun typfield_opt ->
    match typfield_opt with
    | Some (_, plaintyp, _) -> Ok plaintyp
    | None -> fail exp.at "cannot infer type of field"
  in
  let exp_il = Il.Ast.DotE (exp_il, atom) in
  Ok (exp_il, plaintyp.it)

(* Inference of update expressions *)

and infer_upd_exp (ctx : Ctx.t) (exp_b : exp) (path : path) (exp_f : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_b, plaintyp_b = infer_exp ctx exp_b in
  let* path_il, plaintyp_f = elab_path ctx plaintyp_b path in
  let* exp_il_f = elab_exp ctx plaintyp_f exp_f in
  let exp_il = Il.Ast.UpdE (exp_il_b, path_il, exp_il_f) in
  Ok (exp_il, plaintyp_b.it)

(* Inference of length expressions *)

and infer_len_exp (ctx : Ctx.t) (exp : exp) : (Il.Ast.exp' * plaintyp') attempt
    =
  let* exp_il, plaintyp = infer_exp ctx exp in
  let* _plaintyp = infer_as_list ctx plaintyp in
  let exp_il = Il.Ast.LenE exp_il in
  let plaintyp = NumT `NatT in
  Ok (exp_il, plaintyp)

(* Inference of parenthesized expressions *)

and infer_paren_exp (ctx : Ctx.t) (exp : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  infer_exp' ctx exp.at exp.it

(* Inference of tuple expressions *)

and infer_tuple_exp (ctx : Ctx.t) (exps : exp list) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exps_il, plaintyps = infer_exps ctx exps in
  let exp_il = Il.Ast.TupleE exps_il in
  let plaintyp = TupleT plaintyps in
  Ok (exp_il, plaintyp)

(* Inference of call expressions *)

and infer_call_exp (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list)
    : (Il.Ast.exp' * plaintyp') attempt =
  let tparams, params, plaintyp = Ctx.find_dec ctx id in
  check
    (List.length targs = List.length tparams)
    id.at "type arguments do not match";
  let theta = List.combine tparams targs |> Subst.Theta.of_list in
  let params = Subst.subst_params theta params in
  let plaintyp = Subst.subst_plaintyp theta plaintyp in
  let targs_il = List.map (elab_plaintyp ctx) targs in
  check (List.length args = List.length params) id.at "arguments do not match";
  let args_il = List.map2 (elab_arg ctx) params args in
  let exp_il = Il.Ast.CallE (id, targs_il, args_il) in
  Ok (exp_il, plaintyp.it)

(* Inference of iterated expressions *)

and infer_iter_exp (ctx : Ctx.t) (exp : exp) (iter : iter) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il, plaintyp = infer_exp ctx exp in
  let iter_il = elab_iter iter in
  let exp_il = Il.Ast.IterE (exp_il, (iter_il, [])) in
  let plaintyp = IterT (plaintyp, iter) in
  Ok (exp_il, plaintyp)

(* Inference of typed expressions *)

and infer_typ_exp (ctx : Ctx.t) (exp : exp) (plaintyp : plaintyp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il = elab_exp ctx plaintyp exp in
  let _typ_il = elab_plaintyp ctx plaintyp in
  Ok (exp_il.it, plaintyp.it)

(* Elaboration of expression type *)

and fail_cast (at : region) (plaintyp_a : plaintyp) (plaintyp_b : plaintyp) =
  Fail
    ( at,
      "cannot cast "
      ^ El.Print.string_of_plaintyp plaintyp_a
      ^ " to "
      ^ El.Print.string_of_plaintyp plaintyp_b )

and cast_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp)
    (plaintyp_infer : plaintyp) (exp_il : Il.Ast.exp) : Il.Ast.exp attempt =
  if equiv_plaintyp ctx plaintyp_expect plaintyp_infer then Ok exp_il
  else if sub_plaintyp ctx plaintyp_infer plaintyp_expect then
    let typ_il_expect = elab_plaintyp ctx plaintyp_expect in
    let exp_il =
      Il.Ast.CastE (exp_il, typ_il_expect) $$ (exp_il.at, typ_il_expect.it)
    in
    Ok exp_il
  else fail_cast exp_il.at plaintyp_infer plaintyp_expect

and elab_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp) :
    Il.Ast.exp attempt =
  (* Format.printf "=> Elaborate %s, expecting %s\n" *)
  (*   (El.Print.string_of_exp exp) *)
  (*   (El.Print.string_of_plaintyp plaintyp_expect); *)
  (* Expression elaboration is a two-step process:
     - if a type can be inferred without any contextual information,
       match the inferred type with the expected type
     - this may fail for some expressions that require contextual information,
       e.g., notation expressions or expression sequences
     - for such cases, try to elaborate the expression using the expected type *)
  let infer_attempt = infer_exp ctx exp in
  match infer_attempt with
  | Ok (exp_il, plaintyp_infer) ->
      (* Format.printf "==> Inferred %s to have type %s\n" *)
      (*   (Il.Print.string_of_exp exp_il) *)
      (*   (El.Print.string_of_plaintyp plaintyp_infer); *)
      cast_exp ctx plaintyp_expect plaintyp_infer exp_il
  | Fail _ -> (
      (* Format.printf "==> Failed to infer %s\n" *)
      (*   (El.Print.string_of_exp exp); *)
      let kind = kind_of_typ ctx plaintyp_expect in
      let typ_il = elab_plaintyp ctx plaintyp_expect in
      match kind with
      | `Opaque -> elab_exp_plain ctx plaintyp_expect exp
      | `Plain plaintyp ->
          (* Format.printf "===> Elaborate plain expression %s\n" *)
          (*   (El.Print.string_of_plaintyp plaintyp); *)
          elab_exp_plain ctx plaintyp exp
      | `Struct typfields ->
          let* expfields_il = elab_exp_struct ctx typfields exp in
          Ok (Il.Ast.StrE expfields_il $$ (exp.at, typ_il.it))
      | `Variant typcases ->
          (* Format.printf "====> Elaborate variant expression\n"; *)
          let* nottyp_il = elab_exp_variant ctx typcases exp in
          Ok (Il.Ast.CaseE nottyp_il $$ (exp.at, typ_il.it)))

and elab_exps (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exps : exp list) :
    Il.Ast.exp list attempt =
  match exps with
  | [] -> Ok []
  | exp :: exps ->
      let* exp_il = elab_exp ctx plaintyp_expect exp in
      let* exps_il = elab_exps ctx plaintyp_expect exps in
      Ok (exp_il :: exps_il)

(* Elaboration of plain expressions *)

and elab_as_iter (ctx : Ctx.t) (plaintyp : plaintyp) : (plaintyp * iter) attempt
    =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | IterT (plaintyp, iter) -> Ok (plaintyp, iter)
  | _ -> fail plaintyp.at "cannot elaborate type as an iteration"

and elab_as_list (ctx : Ctx.t) (plaintyp : plaintyp) : plaintyp attempt =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | IterT (plaintyp, List) -> Ok plaintyp
  | _ -> fail plaintyp.at "cannot elaborate type as a list"

and elab_as_struct (ctx : Ctx.t) (plaintyp : plaintyp) : typfield list attempt =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | VarT (tid, _) -> (
      let td_opt = Ctx.find_typdef_opt ctx tid in
      match td_opt with
      | Some (Defined (_, `Struct typfields)) -> Ok typfields
      | _ -> fail plaintyp.at "cannot elaborate type as struct")
  | _ -> fail plaintyp.at "cannot elaborate type as struct"

and fail_elab_plain (at : region) (msg : string) =
  fail at ("cannot elaborate expression because " ^ msg)

and elab_exp_plain (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp) :
    Il.Ast.exp attempt =
  let* exp_il = elab_exp_plain' ctx exp.at plaintyp_expect exp.it in
  let typ_il = elab_plaintyp ctx plaintyp_expect in
  Ok (exp_il $$ (exp.at, typ_il.it))

and elab_exp_plain' (ctx : Ctx.t) (at : region) (plaintyp_expect : plaintyp)
    (exp : exp') : Il.Ast.exp' attempt =
  match exp with
  | BoolE _ | NumE _ | TextE _ | VarE _ ->
      fail_elab_plain at "should have been inferred"
  | EpsE -> elab_eps_exp ctx plaintyp_expect
  | ListE exps -> elab_list_exp ctx plaintyp_expect exps
  | ConsE (exp_h, exp_t) -> elab_cons_exp ctx plaintyp_expect exp_h exp_t
  | CatE (exp_l, exp_r) -> elab_cat_exp ctx plaintyp_expect exp_l exp_r
  | ParenE exp -> elab_paren_exp ctx plaintyp_expect exp
  | IterE (exp, iter) -> elab_iter_exp ctx plaintyp_expect exp iter
  | _ -> todo at "elab_exp_plain" (El.Print.string_of_exp (exp $ at))

(* Elaboration of episilon expressions *)

and elab_eps_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) :
    Il.Ast.exp' attempt =
  let* _plaintyp_expect, iter_expect = elab_as_iter ctx plaintyp_expect in
  match iter_expect with
  | Opt -> Ok (Il.Ast.OptE None)
  | List -> Ok (Il.Ast.ListE [])

(* Elaboration of list expressions *)

and elab_list_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exps : exp list) :
    Il.Ast.exp' attempt =
  let* plaintyp_expect, iter_expect = elab_as_iter ctx plaintyp_expect in
  match iter_expect with
  | Opt -> fail_elab_plain no_region "list expression with optional iteration"
  | List ->
      let* exps_il = elab_exps ctx plaintyp_expect exps in
      Ok (Il.Ast.ListE exps_il)

(* Elaboration of cons expressions *)

and elab_cons_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp_h : exp)
    (exp_t : exp) : Il.Ast.exp' attempt =
  let* plaintyp_expect, iter_expect = elab_as_iter ctx plaintyp_expect in
  let* exp_il_h = elab_exp ctx plaintyp_expect exp_h in
  let* exp_il_t =
    elab_exp ctx
      (IterT (plaintyp_expect, iter_expect) $ plaintyp_expect.at)
      exp_t
  in
  Ok (Il.Ast.ConsE (exp_il_h, exp_il_t))

(* Elaboration of concatenation expressions *)

and elab_cat_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp_l : exp)
    (exp_r : exp) : Il.Ast.exp' attempt =
  let* plaintyp_expect, iter_expect = elab_as_iter ctx plaintyp_expect in
  let plaintyp_expect =
    IterT (plaintyp_expect, iter_expect) $ plaintyp_expect.at
  in
  let* exp_il_l = elab_exp ctx plaintyp_expect exp_l in
  let* exp_il_r = elab_exp ctx plaintyp_expect exp_r in
  Ok (Il.Ast.CatE (exp_il_l, exp_il_r))

(* Elaboration of parenthesized expressions *)

and elab_paren_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp) :
    Il.Ast.exp' attempt =
  let* exp_il = elab_exp ctx plaintyp_expect exp in
  Ok exp_il.it

(* Elaboration of iterated expressions *)

and elab_iter_exp (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp)
    (iter : iter) : Il.Ast.exp' attempt =
  let* plaintyp_expect, iter_expect = elab_as_iter ctx plaintyp_expect in
  if iter <> iter_expect then fail_elab_plain exp.at "iteration mismatch"
  else
    let* exp_il = elab_exp ctx plaintyp_expect exp in
    let iter_il_expect = elab_iter iter_expect in
    Ok (Il.Ast.IterE (exp_il, (iter_il_expect, [])))

(* Elaboration of notation expressions *)

and fail_elab_not (at : region) (msg : string) : Il.Ast.notexp attempt =
  Fail (at, "cannot elaborate notation expression because " ^ msg)

and elab_exp_not (ctx : Ctx.t) (typ : typ) (exp : exp) : Il.Ast.notexp attempt =
  let exp = unparen_exp exp in
  match typ with
  | PlainT plaintyp ->
      let mixop = [ []; [] ] in
      let* exp_il = elab_exp ctx plaintyp exp in
      Ok (mixop, [ exp_il ])
  | NotationT nottyp -> (
      match (nottyp.it, exp.it) with
      | AtomT atom_t, AtomE atom_e when atom_t.it <> atom_e.it ->
          fail_elab_not exp.at "atom does not match"
      | AtomT atom_t, AtomE _ ->
          let mixop = [ [ atom_t ] ] in
          Ok (mixop, [])
      | SeqT [], SeqE [] ->
          let mixop = [ [] ] in
          let exps_il = [] in
          Ok (mixop, exps_il)
      | SeqT (typ :: typs), SeqE (exp :: exps) ->
          let* mixop_h, exps_il_h = elab_exp_not ctx typ exp in
          let* mixop_t, exps_il_t =
            elab_exp_not ctx
              (NotationT (SeqT typs $ nottyp.at))
              (SeqE exps $ exp.at)
          in
          let mixop = Mixop.merge mixop_h mixop_t in
          let exps_il = exps_il_h @ exps_il_t in
          Ok (mixop, exps_il)
      | SeqT (_ :: _), SeqE [] -> fail_elab_not exp.at "omitted sequence tail"
      | SeqT [], SeqE (_ :: _) -> fail_elab_not exp.at "expression is not empty"
      | InfixT (_, atom_t, _), InfixE (_, atom_e, _) when atom_t.it <> atom_e.it
        ->
          fail_elab_not exp.at "atoms do not match"
      | InfixT (typ_l, atom_t, typ_r), InfixE (exp_l, _, exp_r) ->
          let* mixop_l, exps_il_l = elab_exp_not ctx typ_l exp_l in
          let* mixop_r, exps_il_r = elab_exp_not ctx typ_r exp_r in
          let mixop_l = Mixop.merge mixop_l [ [ atom_t ] ] in
          let mixop = Mixop.merge mixop_l mixop_r in
          let exps_il = exps_il_l @ exps_il_r in
          Ok (mixop, exps_il)
      | BrackT (atom_t_l, _, atom_t_r), BrackE (atom_e_l, _, atom_e_r)
        when atom_t_l.it <> atom_e_l.it || atom_t_r.it <> atom_e_r.it ->
          fail_elab_not exp.at "atoms do not match"
      | BrackT (atom_t_l, typ, atom_t_r), BrackE (_, exp, _) ->
          let* mixop, exps_il = elab_exp_not ctx typ exp in
          let mixop_l = Mixop.merge [ [ atom_t_l ] ] mixop in
          let mixop = Mixop.merge mixop_l [ [ atom_t_r ] ] in
          Ok (mixop, exps_il)
      | _ -> fail_elab_not exp.at "expression does not match notation")

(* Elaboration of struct expressions *)

and fail_elab_struct (at : region) (msg : string) :
    (Il.Ast.atom * Il.Ast.exp) list attempt =
  Fail (at, "cannot elaborate struct expression because " ^ msg)

and elab_expfields (ctx : Ctx.t) (at : region)
    (typfields : (atom * plaintyp) list) (expfields : (atom * exp) list) :
    (Il.Ast.atom * Il.Ast.exp) list attempt =
  match (typfields, expfields) with
  | [], [] -> Ok []
  | [], (atom_e, _) :: _ ->
      fail_elab_struct atom_e.at "expression has extra fields"
  | _ :: _, [] -> fail_elab_struct at "expression omitted struct fields"
  | (atom_t, _) :: _, (atom_e, _) :: _ when atom_t.it <> atom_e.it ->
      fail_elab_struct atom_e.at "atom does not match"
  | (atom_t, plaintyp) :: typfields, (_, exp) :: expfields ->
      let* exp_il = elab_exp ctx plaintyp exp in
      let* expfields_il = elab_expfields ctx at typfields expfields in
      Ok ((atom_t, exp_il) :: expfields_il)

and elab_exp_struct (ctx : Ctx.t) (typfields : typfield list) (exp : exp) :
    (Il.Ast.atom * Il.Ast.exp) list attempt =
  let typfields =
    List.map (fun (atom, plaintyp, _) -> (atom, plaintyp)) typfields
  in
  match exp.it with
  | StrE expfields ->
      let* expfields_il = elab_expfields ctx exp.at typfields expfields in
      Ok expfields_il
  | _ -> fail_elab_struct exp.at "expression is not a struct"

(* Elaboration of variant expressions *)

and fail_elab_variant (at : region) (msg : string) : Il.Ast.notexp attempt =
  fail at ("cannot elaborate variant case because " ^ msg)

and elab_exp_variant (ctx : Ctx.t) (nottyps : nottyp list) (exp : exp) :
    Il.Ast.notexp attempt =
  let notexps_il =
    List.filter_map
      (fun nottyp ->
        let notexp_il_attempt = elab_exp_not ctx (NotationT nottyp) exp in
        match notexp_il_attempt with
        | Ok notexp_il -> Some notexp_il
        | Fail _ -> None)
      nottyps
  in
  match notexps_il with
  | [ notexp_il ] -> Ok notexp_il
  | [] -> fail_elab_variant exp.at "expression does not match any case"
  | _ -> fail_elab_variant exp.at "expression matches multiple cases"

(* Elaboration of paths *)

and elab_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path) :
    (Il.Ast.path * plaintyp) attempt =
  let* path_il, plaintyp = elab_path' ctx plaintyp_expect path.it in
  let plaintyp = plaintyp $ plaintyp_expect.at in
  let typ_il = elab_plaintyp ctx plaintyp in
  Ok (path_il $$ (path.at, typ_il.it), plaintyp)

and elab_path' (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path') :
    (Il.Ast.path' * plaintyp') attempt =
  match path with
  | RootP -> elab_root_path plaintyp_expect
  | IdxP (path, exp) -> elab_idx_path ctx plaintyp_expect path exp
  | SliceP (path, exp_l, exp_h) ->
      elab_slice_path ctx plaintyp_expect path exp_l exp_h
  | DotP (path, atom) -> elab_dot_path ctx plaintyp_expect path atom

(* Elaboration of root paths *)

and elab_root_path (plaintyp_expect : plaintyp) :
    (Il.Ast.path' * plaintyp') attempt =
  Ok (Il.Ast.RootP, plaintyp_expect.it)

(* Elaboration of index paths *)

and elab_idx_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path)
    (exp : exp) : (Il.Ast.path' * plaintyp') attempt =
  let* path_il, plaintyp = elab_path ctx plaintyp_expect path in
  let* exp_il = elab_exp ctx (NumT `NatT $ exp.at) exp in
  let path_il = Il.Ast.IdxP (path_il, exp_il) in
  let* plaintyp = elab_as_list ctx plaintyp in
  Ok (path_il, plaintyp.it)

(* Elaboration of slice paths *)

and elab_slice_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path)
    (exp_l : exp) (exp_h : exp) : (Il.Ast.path' * plaintyp') attempt =
  let* path_il, plaintyp = elab_path ctx plaintyp_expect path in
  let* exp_il_l = elab_exp ctx (NumT `NatT $ exp_l.at) exp_l in
  let* exp_il_h = elab_exp ctx (NumT `NatT $ exp_h.at) exp_h in
  let path_il = Il.Ast.SliceP (path_il, exp_il_l, exp_il_h) in
  let* _ = elab_as_list ctx plaintyp in
  Ok (path_il, plaintyp.it)

(* Elaboration of dot paths *)

and elab_dot_path (ctx : Ctx.t) (plaintyp_expect : plaintyp) (path : path)
    (atom : atom) : (Il.Ast.path' * plaintyp') attempt =
  let* path_il, plaintyp = elab_path ctx plaintyp_expect path in
  let* typfields = elab_as_struct ctx plaintyp in
  let* plaintyp =
    List.find_opt (fun (atom_t, _, _) -> atom.it = atom_t.it) typfields
    |> fun typfield_opt ->
    match typfield_opt with
    | Some (_, plaintyp, _) -> Ok plaintyp
    | None -> fail atom.at "cannot infer type of field"
  in
  Ok (Il.Ast.DotP (path_il, atom), plaintyp.it)

(* Elaboration of parameters *)

and elab_param (ctx : Ctx.t) (param : param) : Il.Ast.param =
  match param.it with
  | ExpP plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      Il.Ast.ExpP typ_il $ param.at
  | DefP (id, tparams, params, plaintyp) ->
      check
        (List.map it tparams |> distinct ( = ))
        id.at "type parameters are not distinct";
      let ctx_local = ctx in
      let ctx_local = Ctx.add_tparams ctx_local tparams in
      let params_il = List.map (elab_param ctx_local) params in
      let typ_il = elab_plaintyp ctx_local plaintyp in
      Il.Ast.DefP (id, tparams, params_il, typ_il) $ param.at

(* Elaboration of arguments *)

and elab_arg (ctx : Ctx.t) (param : param) (arg : arg) : Il.Ast.arg =
  match (param.it, arg.it) with
  | ExpP plaintyp, ExpA exp ->
      let+ exp_il = elab_exp ctx plaintyp exp in
      Il.Ast.ExpA exp_il $ arg.at
  | DefP (id_p, _, _, _), DefA id_a ->
      check (id_p.it = id_a.it) arg.at "argument does not match parameter";
      Il.Ast.DefA id_a $ arg.at
  | _ -> error arg.at "argument does not match parameter"

(* Elaboration of premises *)

and elab_prem_with_bind (ctx : Ctx.t) (prem : prem) : Ctx.t * Il.Ast.prem option
    =
  let ctx, prem_il_opt = elab_prem ctx prem in
  let prem_il_opt =
    Option.map
      (fun prem_il ->
        let+ prem_il = Bind.bind Bind.bind_prem ctx.venv prem_il in
        prem_il)
      prem_il_opt
  in
  (ctx, prem_il_opt)

and elab_prem (ctx : Ctx.t) (prem : prem) : Ctx.t * Il.Ast.prem option =
  let ctx, prem_il_opt = elab_prem' ctx prem.it in
  let prem_il_opt = Option.map (fun prem_il -> prem_il $ prem.at) prem_il_opt in
  (ctx, prem_il_opt)

and elab_prem' (ctx : Ctx.t) (prem : prem') : Ctx.t * Il.Ast.prem' option =
  let wrap_ctx prem = (ctx, prem) in
  let wrap_some (ctx, prem) = (ctx, Some prem) in
  let wrap_none ctx = (ctx, None) in
  match prem with
  | VarPr (id, plaintyp) -> elab_var_prem ctx id plaintyp |> wrap_none
  | RulePr (id, exp) -> elab_rule_prem ctx id exp |> wrap_some
  | IfPr exp -> elab_if_prem ctx exp |> wrap_some
  | ElsePr -> elab_else_prem () |> wrap_ctx |> wrap_some
  | IterPr (prem, iter) -> elab_iter_prem ctx prem iter |> wrap_some

and elab_prems_with_bind (ctx : Ctx.t) (prems : prem list) :
    Ctx.t * Il.Ast.prem list =
  List.fold_left
    (fun (ctx, prems_il) prem ->
      let ctx, prem_il_opt = elab_prem_with_bind ctx prem in
      match prem_il_opt with
      | Some prem_il -> (ctx, prems_il @ [ prem_il ])
      | None -> (ctx, prems_il))
    (ctx, []) prems

(* Elaboration of variable premises *)

and elab_var_prem (ctx : Ctx.t) (id : id) (plaintyp : plaintyp) : Ctx.t =
  check (valid_tid id) id.at "invalid meta-variable identifier";
  check (not (Ctx.bound_typdef ctx id)) id.at "type already defined";
  let _typ_il = elab_plaintyp ctx plaintyp in
  Ctx.add_metavar ctx id plaintyp

(* Elaboration of rule premises *)

and elab_rule_prem (ctx : Ctx.t) (id : id) (exp : exp) : Ctx.t * Il.Ast.prem' =
  let nottyp, inputs = Ctx.find_rel ctx id in
  let+ notexp_il = elab_exp_not ctx (NotationT nottyp) exp in
  let exps_input, exps_output =
    let exps = notexp_il |> snd in
    List.mapi (fun idx exp -> (idx, exp)) exps
    |> List.partition (fun (idx, _) -> List.mem idx inputs)
    |> fun (exps_input, exps_output) ->
    (List.map snd exps_input, List.map snd exps_output)
  in
  let+ frees_input = Bind.free_exps ctx.venv exps_input in
  if not (VEnv.is_empty frees_input) then
    error exp.at
      (Format.asprintf "rule input has free variable(s): %s"
         (VEnv.to_string frees_input));
  let+ frees_output = Bind.free_exps ctx.venv exps_output in
  let ctx_local = frees_output |> VEnv.bindings |> Ctx.add_vars ctx in
  let prem_il = Il.Ast.RulePr (id, notexp_il) in
  (ctx_local, prem_il)

(* Elaboration of if premises :

   disambiguate `=` of whether it means equality or assignment,
   via dataflow analysis of ordered premises *)

and elab_if_eq_prem (ctx : Ctx.t) (at : region) (optyp : Il.Ast.optyp)
    (exp_il_l : Il.Ast.exp) (exp_il_r : Il.Ast.exp) : Ctx.t * Il.Ast.prem' =
  let+ kind = Bind.free_if_prem ctx.venv at exp_il_l exp_il_r in
  match kind with
  | `Equality ->
      let exp_il =
        Il.Ast.CmpE (`EqOp, optyp, exp_il_l, exp_il_r) $$ (at, Il.Ast.BoolT)
      in
      let prem_il = Il.Ast.IfPr exp_il in
      (ctx, prem_il)
  | `AssignL frees ->
      let ctx = frees |> VEnv.bindings |> Ctx.add_vars ctx in
      let prem_il = Il.Ast.LetPr (exp_il_l, exp_il_r) in
      (ctx, prem_il)
  | `AssignR frees ->
      let ctx = frees |> VEnv.bindings |> Ctx.add_vars ctx in
      let prem_il = Il.Ast.LetPr (exp_il_r, exp_il_l) in
      (ctx, prem_il)

and elab_if_cond_prem (ctx : Ctx.t) (exp_il : Il.Ast.exp) : Il.Ast.prem' =
  let+ frees = Bind.free_exp ctx.venv exp_il in
  if not (VEnv.is_empty frees) then
    error exp_il.at
      (Format.asprintf "condition has free variable(s): %s"
         (VEnv.to_string frees));
  Il.Ast.IfPr exp_il

and elab_if_prem (ctx : Ctx.t) (exp : exp) : Ctx.t * Il.Ast.prem' =
  let+ exp_il = elab_exp ctx (BoolT $ exp.at) exp in
  match exp_il.it with
  | CmpE (`EqOp, optyp, exp_il_l, exp_il_r) ->
      elab_if_eq_prem ctx exp_il.at optyp exp_il_l exp_il_r
  | _ ->
      let prem_il = elab_if_cond_prem ctx exp_il in
      (ctx, prem_il)

(* Elaboration of else premises *)

and elab_else_prem () : Il.Ast.prem' = Il.Ast.ElsePr

(* Elaboration of iterated premises *)

and elab_iter_prem (ctx : Ctx.t) (prem : prem) (iter : iter) :
    Ctx.t * Il.Ast.prem' =
  check
    (match prem.it with VarPr _ | ElsePr -> false | _ -> true)
    prem.at "only rule or if premises can be iterated";
  let iter_il = elab_iter iter in
  let ctx, prem_il_opt = elab_prem ctx prem in
  let prem_il = Option.get prem_il_opt in
  let prem_il = Il.Ast.IterPr (prem_il, (iter_il, [])) in
  (ctx, prem_il)

(* Elaboration of definitions *)

let rec elab_def (ctx : Ctx.t) (def : def) : Ctx.t * Il.Ast.def option =
  let wrap_some (ctx, def) = (ctx, Some def) in
  let wrap_none ctx = (ctx, None) in
  match def.it with
  | SynD syns -> elab_syn_def ctx syns |> wrap_none
  | TypD (id, tparams, deftyp, _hints) ->
      elab_typ_def ctx id tparams deftyp |> wrap_some
  | VarD (id, plaintyp, _hints) -> elab_var_def ctx id plaintyp |> wrap_none
  | RelD (id, nottyp, hints) ->
      elab_rel_def ctx def.at id nottyp hints |> wrap_some
  | RuleD (id_rel, id_rule, exp, prems) ->
      elab_rule_def ctx def.at id_rel id_rule exp prems |> wrap_none
  | DecD (id, tparams, params, plaintyp, _hints) ->
      elab_dec_def ctx def.at id tparams params plaintyp |> wrap_some
  | DefD (id, tparams, args, exp, prems) ->
      elab_def_def ctx def.at id tparams args exp prems |> wrap_none
  | SepD -> ctx |> wrap_none

and elab_defs (ctx : Ctx.t) (defs : def list) : Ctx.t * Il.Ast.def list =
  List.fold_left
    (fun (ctx, defs_il) def ->
      let ctx, def_il_opt = elab_def ctx def in
      match def_il_opt with
      | Some def_il -> (ctx, defs_il @ [ def_il ])
      | None -> (ctx, defs_il))
    (ctx, []) defs

(* Elaboration of type declarations *)

and elab_syn_def (ctx : Ctx.t) (syns : (id * tparam list) list) : Ctx.t =
  List.fold_left
    (fun ctx (id, tparams) ->
      check
        (List.map it tparams |> distinct ( = ))
        id.at "type parameters are not distinct";
      check (valid_tid id) id.at "invalid type identifier";
      let td = TypeDef.Defining tparams in
      let ctx = Ctx.add_typdef ctx id td in
      if tparams = [] then
        let plaintyp = VarT (id, []) $ id.at in
        Ctx.add_metavar ctx id plaintyp
      else ctx)
    ctx syns

(* Elaboration of type definitions *)

and elab_typ_def (ctx : Ctx.t) (id : id) (tparams : tparam list)
    (deftyp : deftyp) : Ctx.t * Il.Ast.def =
  let td_opt = Ctx.find_typdef_opt ctx id in
  let ctx =
    match td_opt with
    | Some (TypeDef.Defining tparams_defining) ->
        let tparams = List.map it tparams in
        let tparams_defining = List.map it tparams_defining in
        check
          (List.length tparams = List.length tparams_defining
          && List.for_all2 ( = ) tparams tparams_defining)
          id.at "type parameters do not match";
        ctx
    | None ->
        check (valid_tid id) id.at "invalid type identifier";
        let td = TypeDef.Defining tparams in
        let ctx = Ctx.add_typdef ctx id td in
        if tparams = [] then
          let plaintyp = VarT (id, []) $ id.at in
          Ctx.add_metavar ctx id plaintyp
        else ctx
    | _ -> error id.at "type was already defined"
  in
  check (List.for_all valid_tid tparams) id.at "invalid type parameter";
  let ctx_local = Ctx.add_tparams ctx tparams in
  let td, deftyp_il = elab_deftyp ctx_local tparams deftyp in
  let def_il = Il.Ast.TypD (id, tparams, deftyp_il) $ deftyp.at in
  let ctx = Ctx.update_typdef ctx id td in
  (ctx, def_il)

(* Elaboration of variable declarations *)

and elab_var_def (ctx : Ctx.t) (id : id) (plaintyp : plaintyp) : Ctx.t =
  check (valid_tid id) id.at "invalid meta-variable identifier";
  check (not (Ctx.bound_typdef ctx id)) id.at "type already defined";
  let _typ_il = elab_plaintyp ctx plaintyp in
  Ctx.add_metavar ctx id plaintyp

(* Elaboration of relation declarations *)

and fetch_rel_input_hint' (len : int) (hintexp : exp) : int list option =
  match hintexp.it with
  | SeqE exps ->
      List.fold_left
        (fun inputs exp ->
          match inputs with
          | Some inputs -> (
              match exp.it with
              | HoleE (`Num input) when input < len -> Some (inputs @ [ input ])
              | _ -> None)
          | None -> None)
        (Some []) exps
  | _ -> None

and fetch_rel_input_hint (at : region) (nottyp_il : Il.Ast.nottyp)
    (hints : hint list) : int list =
  let len = nottyp_il.it |> snd |> List.length in
  let hint_input_default = List.init len Fun.id in
  let hint_input =
    List.find_map
      (fun hint -> if hint.hintid.it = "input" then Some hint.hintexp else None)
      hints
  in
  match hint_input with
  | Some hintexp -> (
      let inputs_opt = fetch_rel_input_hint' len hintexp in
      match inputs_opt with
      | Some [] ->
          error at "malformed input hint: at least one input should be provided"
      | Some inputs -> inputs
      | None ->
          warn at
            (Format.asprintf
               "malformed input hint: should be a sequence of indexed holes \
                %%N (N < %d)"
               len);
          hint_input_default)
  (* If no hint is provided, assume all fields are inputs *)
  | None ->
      warn at "no input hint provided";
      hint_input_default

and elab_rel_def (ctx : Ctx.t) (at : region) (id : id) (nottyp : nottyp)
    (hints : hint list) : Ctx.t * Il.Ast.def =
  let nottyp_il = elab_nottyp ctx (NotationT nottyp) in
  let inputs = fetch_rel_input_hint at nottyp_il hints in
  let ctx = Ctx.add_rel ctx id nottyp inputs in
  let def_il = Il.Ast.RelD (id, nottyp_il, []) $ at in
  (ctx, def_il)

(* Elaboration of rule definitions *)

and elab_rule_input_with_bind (ctx : Ctx.t) (exps_il : (int * Il.Ast.exp) list)
    : Ctx.t * (int * Il.Ast.exp) list =
  let+ frees_input = exps_il |> List.map snd |> Bind.free_exps ctx.venv in
  let ctx = frees_input |> VEnv.bindings |> Ctx.add_vars ctx in
  let idxs, exps_il = List.split exps_il in
  let+ exps_il = Bind.bind Bind.bind_exps ctx.venv exps_il in
  let exps_il = List.combine idxs exps_il in
  (ctx, exps_il)

and elab_rule_output_with_bind (ctx : Ctx.t) (at : region)
    (exps_il : (int * Il.Ast.exp) list) : (int * Il.Ast.exp) list =
  let+ frees_output = exps_il |> List.map snd |> Bind.free_exps ctx.venv in
  if not (VEnv.is_empty frees_output) then
    error at
      (Format.asprintf "rule output has free variable(s): %s"
         (VEnv.to_string frees_output));
  let idxs, exps_il = List.split exps_il in
  let+ exps_il = Bind.bind Bind.bind_exps ctx.venv exps_il in
  List.combine idxs exps_il

and elab_rule_def (ctx : Ctx.t) (at : region) (id_rel : id) (id_rule : id)
    (exp : exp) (prems : prem list) : Ctx.t =
  let nottyp, inputs = Ctx.find_rel ctx id_rel in
  let ctx_local = ctx in
  let+ notexp_il = elab_exp_not ctx_local (NotationT nottyp) exp in
  let mixop, exps_il = notexp_il in
  let exps_il_input, exps_il_output =
    exps_il
    |> List.mapi (fun idx exp -> (idx, exp))
    |> List.partition (fun (idx, _) -> List.mem idx inputs)
  in
  let ctx_local, exps_il_input =
    elab_rule_input_with_bind ctx_local exps_il_input
  in
  let ctx_local, prems_il = elab_prems_with_bind ctx_local prems in
  let exps_il_output =
    elab_rule_output_with_bind ctx_local exp.at exps_il_output
  in
  let notexp_il =
    let exps_il =
      exps_il_input @ exps_il_output
      |> List.sort (fun (idx_a, _) (idx_b, _) -> compare idx_a idx_b)
      |> List.map snd
    in
    (mixop, exps_il)
  in
  let rule = (id_rule, notexp_il, prems_il) $ at in
  Ctx.add_rule ctx id_rel rule

(* Elaboration of function declarations *)

and elab_dec_def (ctx : Ctx.t) (at : region) (id : id) (tparams : tparam list)
    (params : param list) (plaintyp : plaintyp) : Ctx.t * Il.Ast.def =
  check
    (List.map it tparams |> distinct ( = ))
    id.at "type parameters are not distinct";
  let ctx_local = ctx in
  let ctx_local = Ctx.add_tparams ctx_local tparams in
  let params_il = List.map (elab_param ctx_local) params in
  let typ_il = elab_plaintyp ctx_local plaintyp in
  let def_il = Il.Ast.DecD (id, tparams, params_il, typ_il, []) $ at in
  let ctx = Ctx.add_dec ctx id tparams params plaintyp in
  (ctx, def_il)

(* Elaboration of function definitions *)

and elab_def_input_with_bind (ctx : Ctx.t) (params : param list)
    (args : arg list) : Ctx.t * Il.Ast.arg list =
  let args_il = List.map2 (elab_arg ctx) params args in
  let+ frees_input = Bind.free_args ctx.venv args_il in
  let ctx = frees_input |> VEnv.bindings |> Ctx.add_vars ctx in
  let+ args_il = Bind.bind Bind.bind_args ctx.venv args_il in
  (ctx, args_il)

and elab_def_output_with_bind (ctx : Ctx.t) (plaintyp : plaintyp) (exp : exp) :
    Il.Ast.exp =
  let+ exp_il = elab_exp ctx plaintyp exp in
  let+ frees_output = Bind.free_exp ctx.venv exp_il in
  if not (VEnv.is_empty frees_output) then
    error exp_il.at
      (Format.asprintf "output expression has free variable(s): %s"
         (VEnv.to_string frees_output));
  let+ exp_il = Bind.bind Bind.bind_exp ctx.venv exp_il in
  exp_il

and elab_def_def (ctx : Ctx.t) (at : region) (id : id) (tparams : tparam list)
    (args : arg list) (exp : exp) (prems : prem list) : Ctx.t =
  let tparams_expected, params, plaintyp = Ctx.find_dec ctx id in
  check
    (List.length tparams = List.length tparams_expected
    && List.for_all2 ( = ) (List.map it tparams) (List.map it tparams_expected)
    )
    id.at "type arguments do not match";
  check (List.length params = List.length args) at "arguments do not match";
  let ctx_local = ctx in
  let ctx_local = Ctx.add_tparams ctx_local tparams in
  let ctx_local, args_il = elab_def_input_with_bind ctx_local params args in
  let ctx_local, prems_il = elab_prems_with_bind ctx_local prems in
  let exp_il = elab_def_output_with_bind ctx_local plaintyp exp in
  let clause_il = (args_il, exp_il, prems_il) $ at in
  Ctx.add_clause ctx id clause_il

(* Elaboration of spec *)

(* Populate rules to their respective relations *)

let populate_rule (ctx : Ctx.t) (def_il : Il.Ast.def) : Il.Ast.def =
  match def_il.it with
  | Il.Ast.RelD (id, nottyp_il, []) ->
      let rules_il = Ctx.find_rules ctx id in
      Il.Ast.RelD (id, nottyp_il, rules_il) $ def_il.at
  | Il.Ast.RelD _ -> error def_il.at "relation was already populated"
  | _ -> def_il

let populate_rules (ctx : Ctx.t) (spec_il : Il.Ast.spec) : Il.Ast.spec =
  List.map (populate_rule ctx) spec_il

(* Populate clauses to their respective function declarations *)

let populate_clause (ctx : Ctx.t) (def_il : Il.Ast.def) : Il.Ast.def =
  match def_il.it with
  | Il.Ast.DecD (id, tparams_il, params_il, typ_il, []) ->
      let clauses_il = Ctx.find_clauses ctx id in
      Il.Ast.DecD (id, tparams_il, params_il, typ_il, clauses_il) $ def_il.at
  | Il.Ast.DecD _ -> error def_il.at "declaration was already populated"
  | _ -> def_il

let populate_clauses (ctx : Ctx.t) (spec_il : Il.Ast.spec) : Il.Ast.spec =
  List.map (populate_clause ctx) spec_il

let elab_spec (spec : spec) : Il.Ast.spec =
  let ctx = Ctx.init () in
  let ctx, spec_il = elab_defs ctx spec in
  spec_il |> populate_rules ctx |> populate_clauses ctx
