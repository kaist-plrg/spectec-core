open Xl
open El.Ast
open Dom
open Util.Error
open Util.Source

(* Error *)

let error (at : region) (msg : string) = error at "elab" msg

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

(* Backtracking *)

type 'a attempt = Ok of 'a | Fail of region * string

let ( let* ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail _ as fail -> fail

let ( let+ ) (attempt : 'a attempt) (f : 'a -> 'b) : 'b =
  match attempt with Ok a -> f a | Fail (at, msg) -> error at msg

let rec choice = function
  | [] -> Fail (no_region, "all choices failed")
  | f :: fs -> ( match f () with Ok a -> Ok a | Fail _ -> choice fs)

(* Todo *)

let todo (func : string) (msg : string) =
  let msg = Format.asprintf "(TODO : %s) %s\n" func msg in
  Fail (no_region, msg)

(* Parentheses handling *)

let rec unparen_plaintyp (plaintyp : plaintyp) : plaintyp =
  match plaintyp.it with
  | ParenT plaintyp -> unparen_plaintyp plaintyp
  | _ -> plaintyp

let rec unparen_exp (exp : exp) : exp =
  match exp.it with ParenE exp -> unparen_exp exp | _ -> exp

(* Identifiers *)

let strip_var_suffix id =
  let rec is_sub id idx =
    idx = String.length id || (id.[idx] = '_' && is_sub id (idx + 1))
  in
  match (String.index_opt id.it '_', String.index_opt id.it '\'') with
  | None, None -> id
  | Some idx, None when is_sub id.it idx -> id
  | None, Some idx | Some idx, None -> String.sub id.it 0 idx $ id.at
  | Some idx_a, Some idx_b -> String.sub id.it 0 (min idx_a idx_b) $ id.at

let valid_tid (id : id) = id.it = (strip_var_suffix id).it

(* Iteration *)

let elab_iter (iter : iter) : Il.Ast.iter =
  match iter with Opt -> Il.Ast.Opt | List -> Il.Ast.List

(* Types *)

type kind =
  [ `Plain
  | `Notation of nottyp
  | `Struct of typfield list
  | `Variant of typcase list ]

let kind_of_typ (ctx : Ctx.t) (typ : Il.Ast.typ) : kind =
  match typ.it with
  | VarT (tid, _) -> (
      let td = Ctx.find_typdef ctx tid in
      match td with
      | Defined (_, deftyp) -> (
          match deftyp.it with
          | NotationT nottyp -> `Notation nottyp
          | StructT typfields -> `Struct typfields
          | VariantT typcases -> `Variant typcases)
      | _ -> `Plain)
  | _ -> `Plain

(* Expansion *)

let rec expand_plaintyp (ctx : Ctx.t) (plaintyp : plaintyp) : plaintyp =
  match plaintyp.it with
  | VarT (tid, _) -> (
      let td = Ctx.find_typdef ctx tid in
      match td with
      | Defined (_, deftyp) -> (
          match deftyp.it with
          | NotationT { it = PlainT plaintyp; _ } ->
              expand_plaintyp ctx plaintyp
          | _ -> plaintyp)
      | _ -> plaintyp)
  | _ -> plaintyp

(* Type equivalence and subtyping *)

let rec equiv_plaintyp (ctx : Ctx.t) (plaintyp_a : plaintyp)
    (plaintyp_b : plaintyp) : bool =
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
  | _, IterT (plaintyp_b, List) -> sub_plaintyp ctx plaintyp_a plaintyp_b
  | _ -> false

(* Plain types *)

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
      let typs_il = List.map (elab_plaintyp ctx) targs in
      let tparams = TypeDef.get_tparams td in
      check
        (List.length tparams = List.length targs)
        tid.at "type arguments do not match";
      Il.Ast.VarT (tid, typs_il)
  | ParenT plaintyp -> elab_plaintyp' ctx plaintyp.it
  | TupleT plaintyps ->
      let typs_il = List.map (elab_plaintyp ctx) plaintyps in
      Il.Ast.TupleT typs_il
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
  let atom, plaintyp, _hints = typfield in
  let typ_il = elab_plaintyp ctx plaintyp in
  (atom, typ_il)

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

(* Expression type inference *)

and fail_infer_as (at : region) (construct : string) =
  Fail (at, "cannot infer type as " ^ construct)

and infer_as_list (ctx : Ctx.t) (plaintyp : plaintyp) : plaintyp attempt =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | IterT (plaintyp, _) -> Ok plaintyp
  | _ -> fail_infer_as plaintyp.at "list"

and fail_infer (at : region) (construct : string) =
  Fail (at, "cannot infer type of " ^ construct)

and infer_exp (ctx : Ctx.t) (exp : exp) : (Il.Ast.exp * plaintyp) attempt =
  let* exp_il, plaintyp = infer_exp' ctx exp in
  let typ_il = elab_plaintyp ctx (plaintyp $ exp.at) in
  Ok (exp_il $$ (exp.at, typ_il.it), plaintyp $ exp.at)

and infer_exp' (ctx : Ctx.t) (exp : exp) : (Il.Ast.exp' * plaintyp') attempt =
  match exp.it with
  | BoolE b -> infer_bool_exp b
  | NumE (_, num) -> infer_num_exp num
  | TextE text -> infer_text_exp text
  | VarE (id, targs) -> infer_var_exp ctx id targs
  | UnE _ -> todo "infer_exp" "UnE"
  | BinE _ -> todo "infer_exp" "BinE"
  | CmpE (exp_l, cmpop, exp_r) -> infer_cmpop_exp ctx cmpop exp_l exp_r
  | EpsE -> fail_infer exp.at "empty sequence"
  | ListE exps -> infer_list_exp ctx exp.at exps
  | IdxE (exp_b, exp_i) -> infer_idx_exp ctx exp_b exp_i
  | SliceE (exp_b, exp_l, exp_h) -> infer_slice_exp ctx exp_b exp_l exp_h
  | UpdE _ -> todo "infer_exp" "UpdE"
  | StrE _ -> fail_infer exp.at "record expression"
  | DotE _ -> todo "infer_exp" "DotE"
  | CatE _ -> todo "infer_exp" "CatE"
  | MemE _ -> todo "infer_exp" "MemE"
  | LenE exp -> infer_len_exp ctx exp
  | ParenE exp -> infer_paren_exp ctx exp
  | TupleE exps -> infer_tuple_exp ctx exps
  | CallE (id, targs, args) -> infer_call_exp ctx id targs args
  | IterE (exp, iter) -> infer_iter_exp ctx exp iter
  | TypE (exp, plaintyp) -> infer_typed_exp ctx exp plaintyp
  | ArithE exp -> infer_arith_exp ctx exp
  | AtomE _ -> fail_infer exp.at "atom"
  | SeqE exps -> infer_seq_exp ctx exp.at exps
  | InfixE _ -> fail_infer exp.at "infix expression"
  | BrackE _ -> fail_infer exp.at "bracket expression"
  | HoleE _ -> error exp.at "misplaced hole"
  | FuseE _ -> error exp.at "misplaced token concatenation"
  | UnparenE _ -> error exp.at "misplaced unparenthesize"
  | LatexE _ -> error exp.at "misplaced LaTeX literal"

and infer_exps (ctx : Ctx.t) (exps : exp list) :
    (Il.Ast.exp list * plaintyp list) attempt =
  match exps with
  | [] -> Ok ([], [])
  | exp :: exps ->
      let* exp_il, plaintyp = infer_exp ctx exp in
      let* exps_il, plaintyps = infer_exps ctx exps in
      Ok (exp_il :: exps_il, plaintyp :: plaintyps)

(* Boolean expressions *)

and infer_bool_exp (b : bool) : (Il.Ast.exp' * plaintyp') attempt =
  let exp_il = Il.Ast.BoolE b in
  let plaintyp = BoolT in
  Ok (exp_il, plaintyp)

(* Number expressions *)

and infer_num_exp (num : Num.t) : (Il.Ast.exp' * plaintyp') attempt =
  let exp_il = Il.Ast.NumE num in
  let plaintyp = NumT (Num.to_typ num) in
  Ok (exp_il, plaintyp)

(* Text expressions *)

and infer_text_exp (text : string) : (Il.Ast.exp' * plaintyp') attempt =
  let exp_il = Il.Ast.TextE text in
  let plaintyp = TextT in
  Ok (exp_il, plaintyp)

(* Variable expressions *)

and infer_var_exp (ctx : Ctx.t) (id : id) (targs : targ list) :
    (Il.Ast.exp' * plaintyp') attempt =
  let tid = strip_var_suffix id in
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

(* Comparison expressions *)

and infer_cmpop_exp_poly (ctx : Ctx.t) (cmpop : Bool.cmpop) (exp_l : exp)
    (exp_r : exp) : (Il.Ast.exp' * plaintyp') attempt =
  choice
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

and infer_cmpop_exp (ctx : Ctx.t) (cmpop : cmpop) (exp_l : exp) (exp_r : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  match cmpop with
  | #Bool.cmpop as cmpop -> infer_cmpop_exp_poly ctx cmpop exp_l exp_r
  | #Num.cmpop -> todo "infer_cmpop_exp" "Num.cmpop"

(* List expressions *)

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

(* Index expressions *)

and infer_idx_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_b, plaintyp_b = infer_exp ctx exp_b in
  let* plaintyp = infer_as_list ctx plaintyp_b in
  let* exp_il_i = elab_exp ctx (NumT `NatT $ exp_i.at) exp_i in
  let exp_il = Il.Ast.IdxE (exp_il_b, exp_il_i) in
  Ok (exp_il, plaintyp.it)

(* Slice expressions *)

and infer_slice_exp (ctx : Ctx.t) (exp_b : exp) (exp_l : exp) (exp_h : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il_b, plaintyp_b = infer_exp ctx exp_b in
  let* plaintyp = infer_as_list ctx plaintyp_b in
  let* exp_il_l = elab_exp ctx (NumT `NatT $ exp_l.at) exp_l in
  let* exp_il_h = elab_exp ctx (NumT `NatT $ exp_h.at) exp_h in
  let exp_il = Il.Ast.SliceE (exp_il_b, exp_il_l, exp_il_h) in
  Ok (exp_il, plaintyp.it)

(* Length expressions *)

and infer_len_exp (ctx : Ctx.t) (exp : exp) : (Il.Ast.exp' * plaintyp') attempt
    =
  let* exp_il, plaintyp = infer_exp ctx exp in
  let* _plaintyp = infer_as_list ctx plaintyp in
  let exp_il = Il.Ast.LenE exp_il in
  let typ = NumT `NatT in
  Ok (exp_il, typ)

(* Parenthesized expressions *)

and infer_paren_exp (ctx : Ctx.t) (exp : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  infer_exp' ctx exp

(* Tuple expressions *)

and infer_tuple_exp (ctx : Ctx.t) (exps : exp list) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exps_il, plaintyps = infer_exps ctx exps in
  let exp_il = Il.Ast.TupleE exps_il in
  let plaintyp = TupleT plaintyps in
  Ok (exp_il, plaintyp)

(* Call expressions *)

and infer_call_exp (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list)
    : (Il.Ast.exp' * plaintyp') attempt =
  let tparams, params, plaintyp = Ctx.find_dec ctx id in
  check
    (List.length targs = List.length tparams)
    id.at "type arguments do not match";
  let targs_il = List.map (elab_plaintyp ctx) targs in
  check (List.length args = List.length params) id.at "arguments do not match";
  let args_il = List.map2 (elab_arg ctx) params args in
  let exp_il = Il.Ast.CallE (id, targs_il, args_il) in
  Ok (exp_il, plaintyp.it)

(* Iterated expressions *)

and infer_iter_exp (ctx : Ctx.t) (exp : exp) (iter : iter) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il, plaintyp = infer_exp ctx exp in
  let iter_il = elab_iter iter in
  let exp_il = Il.Ast.IterE (exp_il, (iter_il, [])) in
  let plaintyp = IterT (plaintyp, iter) in
  Ok (exp_il, plaintyp)

(* Typed expressions *)

and infer_typed_exp (ctx : Ctx.t) (exp : exp) (plaintyp : plaintyp) :
    (Il.Ast.exp' * plaintyp') attempt =
  let* exp_il = elab_exp ctx plaintyp exp in
  let _typ_il = elab_plaintyp ctx plaintyp in
  Ok (exp_il.it, plaintyp.it)

(* Arithmetic expressions *)

and infer_arith_exp (ctx : Ctx.t) (exp : exp) :
    (Il.Ast.exp' * plaintyp') attempt =
  infer_exp' ctx exp

(* Sequence expressions *)

and infer_seq_exp (ctx : Ctx.t) (at : region) (exps : exp list) :
    (Il.Ast.exp' * plaintyp') attempt =
  match exps with
  | [] -> fail_infer at "empty sequence"
  | exp :: exps ->
      let* exp_il, plaintyp = infer_exp ctx exp in
      let* exps_il, plaintyps = infer_exps ctx exps in
      if List.for_all (equiv_plaintyp ctx plaintyp) plaintyps then
        let exp_il = Il.Ast.ListE (exp_il :: exps_il) in
        let plaintyp = IterT (plaintyp, List) in
        Ok (exp_il, plaintyp)
      else fail_infer at "sequence with heterogeneous elements"

(* Expression type elaboration *)

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
  (* Expression elaboration is a two-step process:
     - if a type can be inferred without any contextual information,
       match the inferred type with the expected type
     - this may fail for some expressions that require contextual information,
       e.g., notation expressions or expression sequences
     - for such cases, try to elaborate the expression using the expected type *)
  let infer_attempt = infer_exp ctx exp in
  match infer_attempt with
  | Ok (exp_il, plaintyp_infer) ->
      cast_exp ctx plaintyp_expect plaintyp_infer exp_il
  | Fail _ -> (
      let typ_il = elab_plaintyp ctx plaintyp_expect in
      let kind = kind_of_typ ctx typ_il in
      match kind with
      | `Plain -> elab_exp_plain ctx plaintyp_expect exp
      | `Notation nottyp ->
          let* nottyp_il = elab_exp_not ctx nottyp exp in
          Ok (Il.Ast.CaseE nottyp_il $$ (exp.at, typ_il.it))
      | `Struct typfields ->
          let* expfields_il = elab_exp_struct ctx typfields exp in
          Ok (Il.Ast.StrE expfields_il $$ (exp.at, typ_il.it))
      | `Variant typcases ->
          let* nottyp_il = elab_exp_variant ctx typcases exp in
          Ok (Il.Ast.CaseE nottyp_il $$ (exp.at, typ_il.it)))

(* Iterated expressions *)

and elab_exp_iter (ctx : Ctx.t) (at : region) (plaintyp_expect : plaintyp)
    (iter_expect : iter) (exps : exp list) : Il.Ast.exp attempt =
  let* exp_il = elab_exp_iter' ctx plaintyp_expect iter_expect exps in
  let typ_il =
    elab_plaintyp ctx (IterT (plaintyp_expect, iter_expect) $ plaintyp_expect.at)
  in
  Ok (exp_il $$ (at, typ_il.it))

and elab_exp_iter' (ctx : Ctx.t) (plaintyp_expect : plaintyp)
    (iter_expect : iter) (exps : exp list) : Il.Ast.exp' attempt =
  match (exps, iter_expect) with
  | [], Opt -> Ok (Il.Ast.OptE None)
  | [ exp ], Opt ->
      let* exp_il = elab_exp ctx plaintyp_expect exp in
      Ok (Il.Ast.OptE (Some exp_il))
  | _, Opt -> assert false
  | [], List -> Ok (Il.Ast.ListE [])
  | exp :: exps, List ->
      let* exp_il_h =
        elab_exp ctx
          (IterT (plaintyp_expect, iter_expect) $ plaintyp_expect.at)
          exp
      in
      let at = over_region (after_region exp.at :: List.map at exps) in
      let* exp_il_t = elab_exp_iter ctx at plaintyp_expect iter_expect exps in
      Ok (Il.Ast.CatE (exp_il_h, exp_il_t))

(* Plain expressions *)

and fail_elab_as (at : region) (construct : string) =
  Fail (at, "cannot elaborate expression as " ^ construct)

and elab_as_iter (ctx : Ctx.t) (plaintyp : plaintyp) : (plaintyp * iter) attempt
    =
  let plaintyp = expand_plaintyp ctx plaintyp in
  match plaintyp.it with
  | IterT (plaintyp, iter) -> Ok (plaintyp, iter)
  | _ -> fail_elab_as plaintyp.at "iteration"

and fail_elab_plain (at : region) (msg : string) : Il.Ast.exp attempt =
  Fail (at, "cannot elaborate expression because" ^ msg)

and elab_exp_plain (ctx : Ctx.t) (plaintyp_expect : plaintyp) (exp : exp) :
    Il.Ast.exp attempt =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ | VarE _ ->
      fail_elab_plain exp.at "should have been inferred"
  | ParenE exp ->
      let* exp_il = elab_exp ctx plaintyp_expect exp in
      let typ_il = exp_il.note in
      Ok (exp_il.it $$ (exp.at, typ_il))
  | IterE (exp, iter) ->
      let* plaintyp_expect, iter_expect = elab_as_iter ctx plaintyp_expect in
      if iter <> iter_expect then fail_elab_plain exp.at "iteration mismatch"
      else
        let* exp_il = elab_exp ctx plaintyp_expect exp in
        let typ_il = exp_il.note $ exp_il.at in
        let iter_il_expect = elab_iter iter_expect in
        let exp_il = Il.Ast.IterE (exp_il, (iter_il_expect, [])) in
        let typ_il = Il.Ast.IterT (typ_il, iter_il_expect) in
        Ok (exp_il $$ (exp.at, typ_il))
  | SeqE exps ->
      let* plaintyp_expect, iter_expect = elab_as_iter ctx plaintyp_expect in
      elab_exp_iter ctx exp.at plaintyp_expect iter_expect exps
  | _ -> todo "elab_exp_plain" (El.Print.string_of_exp exp)

(* Notation expressions *)

and fail_elab_not (at : region) (msg : string) : Il.Ast.notexp attempt =
  Fail (at, "cannot elaborate notation expression because" ^ msg)

and elab_exp_not (ctx : Ctx.t) (nottyp : nottyp) (exp : exp) :
    Il.Ast.notexp attempt =
  let exp = unparen_exp exp in
  match (nottyp.it, exp.it) with
  | PlainT plaintyp, _ ->
      let mixop = [ []; [] ] in
      let* exp_il = elab_exp ctx plaintyp exp in
      Ok (mixop, [ exp_il ])
  | AtomT atom_t, AtomE atom_e when atom_t.it <> atom_e.it ->
      fail_elab_not exp.at "atom does not match"
  | AtomT atom_t, AtomE _ ->
      let mixop = [ [ atom_t ] ] in
      Ok (mixop, [])
  | SeqT [], SeqE [] ->
      let mixop = [ [] ] in
      let exps_il = [] in
      Ok (mixop, exps_il)
  | SeqT (nottyp :: nottyps), SeqE (exp :: exps) ->
      let* mixop_h, exps_il_h = elab_exp_not ctx nottyp exp in
      let* mixop_t, exps_il_t =
        elab_exp_not ctx (SeqT nottyps $ nottyp.at) (SeqE exps $ exp.at)
      in
      let mixop = Mixop.merge mixop_h mixop_t in
      let exps_il = exps_il_h @ exps_il_t in
      Ok (mixop, exps_il)
  | SeqT (_ :: _), SeqE [] -> fail_elab_not exp.at "omitted sequence tail"
  | SeqT [], SeqE (_ :: _) -> fail_elab_not exp.at "expression is not empty"
  | InfixT (_, atom_t, _), InfixE (_, atom_e, _) when atom_t.it <> atom_e.it ->
      fail_elab_not exp.at "atoms do not match"
  | InfixT (nottyp_l, atom_t, nottyp_r), InfixE (exp_l, _, exp_r) ->
      let* mixop_l, exps_il_l = elab_exp_not ctx nottyp_l exp_l in
      let* mixop_r, exps_il_r = elab_exp_not ctx nottyp_r exp_r in
      let mixop_l = Mixop.merge mixop_l [ [ atom_t ] ] in
      let mixop = Mixop.merge mixop_l mixop_r in
      let exps_il = exps_il_l @ exps_il_r in
      Ok (mixop, exps_il)
  | BrackT (atom_t_l, _, atom_t_r), BrackE (atom_e_l, _, atom_e_r)
    when atom_t_l.it <> atom_e_l.it || atom_t_r.it <> atom_e_r.it ->
      fail_elab_not exp.at "atoms do not match"
  | BrackT (atom_t_l, nottyp, atom_t_r), BrackE (_, exp, _) ->
      let* mixop, exps_il = elab_exp_not ctx nottyp exp in
      let mixop_l = Mixop.merge [ [ atom_t_l ] ] mixop in
      let mixop = Mixop.merge mixop_l [ [ atom_t_r ] ] in
      Ok (mixop, exps_il)
  | _ -> fail_elab_not exp.at "expression does not match notation"

(* Struct expressions *)

and fail_elab_struct (at : region) (msg : string) :
    (Il.Ast.atom * Il.Ast.exp) list attempt =
  Fail (at, "cannot elaborate struct expression because" ^ msg)

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

(* Variant expressions *)

and fail_elab_variant (at : region) (msg : string) : Il.Ast.notexp attempt =
  Fail (at, "cannot elaborate variant case because" ^ msg)

and elab_exp_variant (ctx : Ctx.t) (typcases : typcase list) (exp : exp) :
    Il.Ast.notexp attempt =
  let notexps_il =
    List.filter_map
      (fun (nottyp, _) ->
        let notexp_il_attempt = elab_exp_not ctx nottyp exp in
        match notexp_il_attempt with
        | Ok notexp_il -> Some notexp_il
        | Fail _ -> None)
      typcases
  in
  match notexps_il with
  | [ notexp_il ] -> Ok notexp_il
  | [] -> fail_elab_variant exp.at "expression does not match any case"
  | _ -> fail_elab_variant exp.at "expression matches multiple cases"

(* Parameters *)

and elab_param (ctx : Ctx.t) (param : param) : Il.Ast.param =
  match param.it with
  | ExpP plaintyp ->
      let typ_il = elab_plaintyp ctx plaintyp in
      Il.Ast.ExpP typ_il $ param.at
  | DefP (id, tparams, params, plaintyp) ->
      check
        (List.map it tparams |> distinct ( = ))
        id.at "type parameters are not distinct";
      let params_il = List.map (elab_param ctx) params in
      let typ_il = elab_plaintyp ctx plaintyp in
      Il.Ast.DefP (id, tparams, params_il, typ_il) $ param.at

(* Arguments *)

and elab_arg (ctx : Ctx.t) (param : param) (arg : arg) : Il.Ast.arg =
  match (param.it, arg.it) with
  | ExpP plaintyp, ExpA exp ->
      let+ exp_il = elab_exp ctx plaintyp exp in
      Il.Ast.ExpA exp_il $ arg.at
  | DefP (id_p, _, _, _), DefA id_a ->
      check (id_p.it = id_a.it) arg.at "argument does not match parameter";
      Il.Ast.DefA id_a $ arg.at
  | _ -> error arg.at "argument does not match parameter"

(* Premises *)

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
  | RulePr (id, exp) -> elab_rule_prem ctx id exp |> wrap_ctx |> wrap_some
  | IfPr exp -> elab_if_prem ctx exp |> wrap_ctx |> wrap_some
  | ElsePr -> elab_else_prem () |> wrap_ctx |> wrap_some
  | _ ->
      let+ _ = todo "elab_prem" (El.Print.string_of_prem (prem $ no_region)) in
      assert false

and elab_prems (ctx : Ctx.t) (prems : prem list) : Ctx.t * Il.Ast.prem list =
  List.fold_left
    (fun (ctx, prems_il) prem ->
      let ctx, prem_il_opt = elab_prem ctx prem in
      match prem_il_opt with
      | Some prem_il -> (ctx, prems_il @ [ prem_il ])
      | None -> (ctx, prems_il))
    (ctx, []) prems

(* Variable premises *)

and elab_var_prem (ctx : Ctx.t) (id : id) (plaintyp : plaintyp) : Ctx.t =
  check (valid_tid id) id.at "invalid meta-variable identifier";
  check (not (Ctx.bound_typdef ctx id)) id.at "type already defined";
  let _typ_il = elab_plaintyp ctx plaintyp in
  Ctx.add_metavar ctx id.it plaintyp

(* Rule premises *)

and elab_rule_prem (ctx : Ctx.t) (id : id) (exp : exp) : Il.Ast.prem' =
  let nottyp = Ctx.find_rel ctx id in
  let+ notexp_il = elab_exp_not ctx nottyp exp in
  Il.Ast.RulePr (id, notexp_il)

(* If premises *)

and elab_if_prem (ctx : Ctx.t) (exp : exp) : Il.Ast.prem' =
  let+ exp_il = elab_exp ctx (BoolT $ exp.at) exp in
  Il.Ast.IfPr exp_il

(* Else premises *)

and elab_else_prem () : Il.Ast.prem' = Il.Ast.ElsePr

(* Definitions *)

let rec elab_def (ctx : Ctx.t) (def : def) : Ctx.t * Il.Ast.def option =
  let wrap_some (ctx, def) = (ctx, Some def) in
  let wrap_none ctx = (ctx, None) in
  match def.it with
  | SynD (id, tparams) -> elab_syn_def ctx id tparams |> wrap_none
  | TypD (id, tparams, deftyp, _hints) ->
      elab_typ_def ctx id tparams deftyp |> wrap_some
  | VarD (id, plaintyp, _hints) -> elab_var_def ctx id plaintyp |> wrap_none
  | RelD (id, nottyp, _hints) -> elab_rel_def ctx def.at id nottyp |> wrap_some
  | RuleD (id_rel, id_rule, exp, prems) ->
      elab_rule_def ctx def.at id_rel id_rule exp prems |> wrap_none
  | DecD (id, tparams, params, plaintyp, _hints) ->
      elab_dec_def ctx def.at id tparams params plaintyp |> wrap_some
  | DefD (id, targs, args, exp, prems) ->
      elab_def_def ctx def.at id targs args exp prems |> wrap_none
  | SepD -> ctx |> wrap_none

and elab_defs (ctx : Ctx.t) (defs : def list) : Ctx.t * Il.Ast.def list =
  List.fold_left
    (fun (ctx, defs_il) def ->
      let ctx, def_il_opt = elab_def ctx def in
      match def_il_opt with
      | Some def_il -> (ctx, defs_il @ [ def_il ])
      | None -> (ctx, defs_il))
    (ctx, []) defs

(* Type declarations *)

and elab_syn_def (ctx : Ctx.t) (id : id) (tparams : tparam list) : Ctx.t =
  check
    (List.map it tparams |> distinct ( = ))
    id.at "type parameters are not distinct";
  check (valid_tid id) id.at "invalid type identifier";
  let td = TypeDef.Defining tparams in
  let ctx = Ctx.add_typdef ctx id.it td in
  if tparams = [] then
    let plaintyp = VarT (id, []) $ id.at in
    Ctx.add_metavar ctx id.it plaintyp
  else ctx

(* Type definitions *)

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
        let ctx = Ctx.add_typdef ctx id.it td in
        if tparams = [] then
          let plaintyp = VarT (id, []) $ id.at in
          Ctx.add_metavar ctx id.it plaintyp
        else ctx
    | _ -> error id.at "type was already defined"
  in
  check (List.for_all valid_tid tparams) id.at "invalid type parameter";
  let ctx_local = Ctx.add_tparams ctx tparams in
  let deftyp_il = elab_deftyp ctx_local deftyp in
  let def_il = Il.Ast.TypD (id, tparams, deftyp_il) $ deftyp.at in
  let td = TypeDef.Defined (tparams, deftyp) in
  let ctx = Ctx.update_typdef ctx id td in
  (ctx, def_il)

(* Variable declarations *)

and elab_var_def (ctx : Ctx.t) (id : id) (plaintyp : plaintyp) : Ctx.t =
  check (valid_tid id) id.at "invalid meta-variable identifier";
  check (not (Ctx.bound_typdef ctx id)) id.at "type already defined";
  let _typ_il = elab_plaintyp ctx plaintyp in
  Ctx.add_metavar ctx id.it plaintyp

(* Relation declarations *)

and elab_rel_def (ctx : Ctx.t) (at : region) (id : id) (nottyp : nottyp) :
    Ctx.t * Il.Ast.def =
  let nottyp_il = elab_nottyp ctx nottyp in
  let def_il = Il.Ast.RelD (id, nottyp_il, []) $ at in
  let ctx = Ctx.add_rel ctx id.it nottyp in
  (ctx, def_il)

(* Rule definitions *)

and elab_rule_def (ctx : Ctx.t) (at : region) (id_rel : id) (id_rule : id)
    (exp : exp) (prems : prem list) : Ctx.t =
  let nottyp = Ctx.find_rel ctx id_rel in
  let ctx_local = ctx in
  let+ notexp_il = elab_exp_not ctx_local nottyp exp in
  let _ctx_local, prems_il = elab_prems ctx_local prems in
  let rule = (id_rule, notexp_il, prems_il) $ at in
  Ctx.add_rule ctx id_rel.it rule

(* Function declarations *)

and elab_dec_def (ctx : Ctx.t) (at : region) (id : id) (tparams : tparam list)
    (params : param list) (plaintyp : plaintyp) : Ctx.t * Il.Ast.def =
  check
    (List.map it tparams |> distinct ( = ))
    id.at "type parameters are not distinct";
  let params_il = List.map (elab_param ctx) params in
  let typ_il = elab_plaintyp ctx plaintyp in
  let def_il = Il.Ast.DecD (id, tparams, params_il, typ_il, []) $ at in
  let ctx = Ctx.add_dec ctx id.it tparams params plaintyp in
  (ctx, def_il)

(* Function definitions *)

and elab_def_def (ctx : Ctx.t) (at : region) (id : id) (targs : plaintyp list)
    (args : arg list) (exp : exp) (prems : prem list) : Ctx.t =
  let tparams, params, plaintyp = Ctx.find_dec ctx id in
  check
    (List.length targs = List.length tparams)
    at "type arguments do not match";
  check (List.length params = List.length args) at "arguments do not match";
  let ctx_local = ctx in
  let args_il = List.map2 (elab_arg ctx_local) params args in
  let+ exp_il = elab_exp ctx_local plaintyp exp in
  let _ctx_local, prems_il = elab_prems ctx_local prems in
  let clause = (args_il, exp_il, prems_il) $ at in
  Ctx.add_clause ctx id.it clause

(* Spec *)

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
