(* Structural equality for EL ASTs, ignoring source positions. *)

open Xl
open Types

(* Identifiers *)

let eq_id (a : id) (b : id) : bool = a.it = b.it

(* Atoms *)

let eq_atom (a : atom) (b : atom) : bool = Atom.eq a.it b.it

(* Iterators *)

let eq_iter (a : iter) (b : iter) : bool = a = b

(* Types *)

let rec eq_typ (a : typ) (b : typ) : bool =
  match (a, b) with
  | PlainT pa, PlainT pb -> eq_plaintyp pa pb
  | NotationT na, NotationT nb -> eq_nottyp na nb
  | _ -> false

and eq_typs (a : typ list) (b : typ list) : bool = List.equal eq_typ a b

and eq_plaintyp (a : plaintyp) (b : plaintyp) : bool =
  match (a.it, b.it) with
  | BoolT, BoolT -> true
  | NumT na, NumT nb -> na = nb
  | TextT, TextT -> true
  | VarT (ida, targs_a), VarT (idb, targs_b) ->
      eq_id ida idb && eq_targs targs_a targs_b
  | ParenT pa, ParenT pb -> eq_plaintyp pa pb
  | TupleT psa, TupleT psb -> eq_plaintyps psa psb
  | IterT (pa, ia), IterT (pb, ib) -> eq_plaintyp pa pb && eq_iter ia ib
  | _ -> false

and eq_plaintyps (a : plaintyp list) (b : plaintyp list) : bool =
  List.equal eq_plaintyp a b

and eq_nottyp (a : nottyp) (b : nottyp) : bool =
  match (a.it, b.it) with
  | AtomT aa, AtomT ab -> eq_atom aa ab
  | SeqT ta, SeqT tb -> eq_typs ta tb
  | InfixT (la, aa, ra), InfixT (lb, ab, rb) ->
      eq_typ la lb && eq_atom aa ab && eq_typ ra rb
  | BrackT (la, ta, ra), BrackT (lb, tb, rb) ->
      eq_atom la lb && eq_typ ta tb && eq_atom ra rb
  | _ -> false

and eq_deftyp (a : deftyp) (b : deftyp) : bool =
  match (a.it, b.it) with
  | PlainTD pa, PlainTD pb -> eq_plaintyp pa pb
  | StructTD fa, StructTD fb -> List.equal eq_typfield fa fb
  | VariantTD ca, VariantTD cb -> List.equal eq_typcase ca cb
  | _ -> false

and eq_typfield (a, ta, ha) (b, tb, hb) =
  eq_atom a b && eq_plaintyp ta tb && eq_hints ha hb

and eq_typcase (ta, ha) (tb, hb) = eq_typ ta tb && eq_hints ha hb

(* Type arguments *)

and eq_targ (a : targ) (b : targ) : bool = eq_plaintyp a b
and eq_targs (a : targ list) (b : targ list) : bool = List.equal eq_targ a b

(* Operators *)

and eq_unop (a : unop) (b : unop) : bool = a = b
and eq_binop (a : binop) (b : binop) : bool = a = b
and eq_cmpop (a : cmpop) (b : cmpop) : bool = a = b

(* Expressions *)

and eq_exp (a : exp) (b : exp) : bool =
  match (a.it, b.it) with
  | BoolE ba, BoolE bb -> ba = bb
  | NumE (opa, na), NumE (opb, nb) -> opa = opb && na = nb
  | TextE ta, TextE tb -> ta = tb
  | VarE ia, VarE ib -> eq_id ia ib
  | UnE (ua, ea), UnE (ub, eb) -> eq_unop ua ub && eq_exp ea eb
  | BinE (la, oa, ra), BinE (lb, ob, rb) ->
      eq_exp la lb && eq_binop oa ob && eq_exp ra rb
  | CmpE (la, oa, ra), CmpE (lb, ob, rb) ->
      eq_exp la lb && eq_cmpop oa ob && eq_exp ra rb
  | ArithE ea, ArithE eb -> eq_exp ea eb
  | EpsE, EpsE -> true
  | ListE ea, ListE eb -> eq_exps ea eb
  | ConsE (la, ra), ConsE (lb, rb) -> eq_exp la lb && eq_exp ra rb
  | CatE (la, ra), CatE (lb, rb) -> eq_exp la lb && eq_exp ra rb
  | IdxE (ba, ia), IdxE (bb, ib) -> eq_exp ba bb && eq_exp ia ib
  | SliceE (ba, la, ha), SliceE (bb, lb, hb) ->
      eq_exp ba bb && eq_exp la lb && eq_exp ha hb
  | LenE ea, LenE eb -> eq_exp ea eb
  | MemE (ea, sa), MemE (eb, sb) -> eq_exp ea eb && eq_exp sa sb
  | StrE fa, StrE fb ->
      List.equal (fun (aa, ea) (ab, eb) -> eq_atom aa ab && eq_exp ea eb) fa fb
  | DotE (ea, aa), DotE (eb, ab) -> eq_exp ea eb && eq_atom aa ab
  | UpdE (ba, pa, ea), UpdE (bb, pb, eb) ->
      eq_exp ba bb && eq_path pa pb && eq_exp ea eb
  | ParenE ea, ParenE eb -> eq_exp ea eb
  | TupleE ea, TupleE eb -> eq_exps ea eb
  | CallE (ia, tsa, asa), CallE (ib, tsb, asb) ->
      eq_id ia ib && eq_targs tsa tsb && eq_args asa asb
  | IterE (ea, ia), IterE (eb, ib) -> eq_exp ea eb && eq_iter ia ib
  | SubE (ea, pa), SubE (eb, pb) -> eq_exp ea eb && eq_plaintyp pa pb
  | AtomE aa, AtomE ab -> eq_atom aa ab
  | SeqE ea, SeqE eb -> eq_exps ea eb
  | InfixE (la, aa, ra), InfixE (lb, ab, rb) ->
      eq_exp la lb && eq_atom aa ab && eq_exp ra rb
  | BrackE (la, ea, ra), BrackE (lb, eb, rb) ->
      eq_atom la lb && eq_exp ea eb && eq_atom ra rb
  | HoleE ha, HoleE hb -> ha = hb
  | FuseE (la, ra), FuseE (lb, rb) -> eq_exp la lb && eq_exp ra rb
  | UnparenE ea, UnparenE eb -> eq_exp ea eb
  | LatexE sa, LatexE sb -> sa = sb
  | _ -> false

and eq_exps (a : exp list) (b : exp list) : bool = List.equal eq_exp a b

(* Paths *)

and eq_path (a : path) (b : path) : bool =
  match (a.it, b.it) with
  | RootP, RootP -> true
  | IdxP (pa, ea), IdxP (pb, eb) -> eq_path pa pb && eq_exp ea eb
  | SliceP (pa, la, ha), SliceP (pb, lb, hb) ->
      eq_path pa pb && eq_exp la lb && eq_exp ha hb
  | DotP (pa, aa), DotP (pb, ab) -> eq_path pa pb && eq_atom aa ab
  | _ -> false

(* Parameters *)

and eq_param (a : param) (b : param) : bool =
  match (a.it, b.it) with
  | ExpP pa, ExpP pb -> eq_plaintyp pa pb
  | DefP (ia, tpa, psa, pa), DefP (ib, tpb, psb, pb) ->
      eq_id ia ib && eq_tparams tpa tpb && eq_params psa psb
      && eq_plaintyp pa pb
  | _ -> false

and eq_params (a : param list) (b : param list) : bool = List.equal eq_param a b

(* Type parameters *)

and eq_tparam (a : tparam) (b : tparam) : bool = a.it = b.it

and eq_tparams (a : tparam list) (b : tparam list) : bool =
  List.equal eq_tparam a b

(* Arguments *)

and eq_arg (a : arg) (b : arg) : bool =
  match (a.it, b.it) with
  | ExpA ea, ExpA eb -> eq_exp ea eb
  | DefA ia, DefA ib -> eq_id ia ib
  | _ -> false

and eq_args (a : arg list) (b : arg list) : bool = List.equal eq_arg a b

(* Premises *)

and eq_prem (a : prem) (b : prem) : bool =
  match (a.it, b.it) with
  | VarPr (ia, pa), VarPr (ib, pb) -> eq_id ia ib && eq_plaintyp pa pb
  | RulePr (ia, ea), RulePr (ib, eb) -> eq_id ia ib && eq_exp ea eb
  | RuleNotPr (ia, ea), RuleNotPr (ib, eb) -> eq_id ia ib && eq_exp ea eb
  | IfPr ea, IfPr eb -> eq_exp ea eb
  | ElsePr, ElsePr -> true
  | IterPr (pa, ia), IterPr (pb, ib) -> eq_prem pa pb && eq_iter ia ib
  | DebugPr ea, DebugPr eb -> eq_exp ea eb
  | _ -> false

and eq_prems (a : prem list) (b : prem list) : bool = List.equal eq_prem a b

(* Hints *)

and eq_hint (a : hint) (b : hint) : bool =
  eq_id a.hintid b.hintid && eq_exp a.hintexp b.hintexp

and eq_hints (a : hint list) (b : hint list) : bool = List.equal eq_hint a b

(* Definitions *)

let eq_def (a : def) (b : def) : bool =
  match (a.it, b.it) with
  | SynD sa, SynD sb ->
      List.equal
        (fun (ia, tpa) (ib, tpb) -> eq_id ia ib && eq_tparams tpa tpb)
        sa sb
  | TypD (ia, tpa, dta, ha), TypD (ib, tpb, dtb, hb) ->
      eq_id ia ib && eq_tparams tpa tpb && eq_deftyp dta dtb && eq_hints ha hb
  | VarD (ia, pa, ha), VarD (ib, pb, hb) ->
      eq_id ia ib && eq_plaintyp pa pb && eq_hints ha hb
  | RelD (ia, na, ha), RelD (ib, nb, hb) ->
      eq_id ia ib && eq_nottyp na nb && eq_hints ha hb
  | RuleD (ria, rua, ea, psa), RuleD (rib, rub, eb, psb) ->
      eq_id ria rib && eq_id rua rub && eq_exp ea eb && eq_prems psa psb
  | BuiltinDecD (ia, tpa, psa, pa, ha), BuiltinDecD (ib, tpb, psb, pb, hb) ->
      eq_id ia ib && eq_tparams tpa tpb && eq_params psa psb
      && eq_plaintyp pa pb && eq_hints ha hb
  | DecD (ia, tpa, psa, pa, ha), DecD (ib, tpb, psb, pb, hb) ->
      eq_id ia ib && eq_tparams tpa tpb && eq_params psa psb
      && eq_plaintyp pa pb && eq_hints ha hb
  | DefD (ia, tpa, asa, ea, psa), DefD (ib, tpb, asb, eb, psb) ->
      eq_id ia ib && eq_tparams tpa tpb && eq_args asa asb && eq_exp ea eb
      && eq_prems psa psb
  | BuiltinGeneratorD (ia, pa, ha), BuiltinGeneratorD (ib, pb, hb) ->
      eq_id ia ib && eq_plaintyp pa pb && eq_hints ha hb
  | PropertyD (ia, psa, ga, ha), PropertyD (ib, psb, gb, hb) ->
      eq_id ia ib && eq_prems psa psb && eq_prem ga gb && eq_hints ha hb
  | SepD, SepD -> true
  | _ -> false

(* [Pp] does not emit [SepD] (paragraph separators are visual metadata,
   not semantic), so the roundtrip comparison strips them before checking
   AST equality. *)
let eq_spec (a : spec) (b : spec) : bool =
  let strip_sep =
    List.filter (fun (d : def) -> match d.it with SepD -> false | _ -> true)
  in
  List.equal eq_def (strip_sep a) (strip_sep b)
