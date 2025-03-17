open Domain.Lib
open El.Ast
open El.Print
open Util.Error
open Util.Source

(* Plain type *)

type t = plaintyp

let to_string t = string_of_plaintyp t

(* Substitution of type variables *)

type theta = t TIdMap.t

let rec subst_typ (theta : theta) (typ : typ) : typ =
  match typ with
  | PlainT plaintyp ->
      let plaintyp = subst_plaintyp theta plaintyp in
      PlainT plaintyp
  | NotationT nottyp ->
      let nottyp = subst_nottyp theta nottyp in
      NotationT nottyp

and subst_typs (theta : theta) (typs : typ list) : typ list =
  List.map (subst_typ theta) typs

and subst_plaintyp (theta : theta) (plaintyp : plaintyp) : plaintyp =
  match plaintyp.it with
  | BoolT | NumT _ | TextT -> plaintyp
  | VarT (tid, targs) -> (
      match TIdMap.find_opt tid theta with
      | Some plaintyp ->
          if targs <> [] then
            error plaintyp.at "elab" "higher-order substitution is disallowed";
          plaintyp
      | None ->
          let targs = subst_targs theta targs in
          VarT (tid, targs) $ plaintyp.at)
  | ParenT plaintyp ->
      let plaintyp = subst_plaintyp theta plaintyp in
      ParenT plaintyp $ plaintyp.at
  | TupleT plaintyps ->
      let plaintyps = List.map (subst_plaintyp theta) plaintyps in
      TupleT plaintyps $ plaintyp.at
  | IterT (plaintyp, iter) ->
      let plaintyp = subst_plaintyp theta plaintyp in
      IterT (plaintyp, iter) $ plaintyp.at

and subst_plaintyps (theta : theta) (plaintyps : plaintyp list) : plaintyp list
    =
  List.map (subst_plaintyp theta) plaintyps

and subst_nottyp (theta : theta) (nottyp : nottyp) : nottyp =
  match nottyp.it with
  | AtomT _ -> nottyp
  | SeqT typs ->
      let typs = subst_typs theta typs in
      SeqT typs $ nottyp.at
  | InfixT (typ_l, atom, typ_r) ->
      let typ_l = subst_typ theta typ_l in
      let typ_r = subst_typ theta typ_r in
      InfixT (typ_l, atom, typ_r) $ nottyp.at
  | BrackT (atom_l, typ, atom_r) ->
      let typ = subst_typ theta typ in
      BrackT (atom_l, typ, atom_r) $ nottyp.at

and subst_nottyps (theta : theta) (nottyps : nottyp list) : nottyp list =
  List.map (subst_nottyp theta) nottyps

and subst_param (theta : theta) (param : param) : param =
  match param.it with
  | ExpP plaintyp ->
      let plaintyp = subst_plaintyp theta plaintyp in
      ExpP plaintyp $ param.at
  (* (TODO) Capture-avoiding substitution *)
  | DefP (id, tparams, params, plaintyp) ->
      let params = subst_params theta params in
      let plaintyp = subst_plaintyp theta plaintyp in
      DefP (id, tparams, params, plaintyp) $ param.at

and subst_params (theta : theta) (params : param list) : param list =
  List.map (subst_param theta) params

and subst_targ (theta : theta) (targ : targ) : targ = subst_plaintyp theta targ

and subst_targs (theta : theta) (targs : targ list) : targ list =
  List.map (subst_targ theta) targs
