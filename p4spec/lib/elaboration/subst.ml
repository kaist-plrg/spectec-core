open El.Ast
open Envs
open Util.Source
module Theta = TEnv

let rec subst_typ (theta : Theta.t) (typ : typ) : typ =
  match typ with
  | PlainT plaintyp ->
      let plaintyp = subst_plaintyp theta plaintyp in
      PlainT plaintyp
  | NotationT nottyp ->
      let nottyp = subst_nottyp theta nottyp in
      NotationT nottyp

and subst_typs (theta : Theta.t) (typs : typ list) : typ list =
  List.map (subst_typ theta) typs

and subst_plaintyp (theta : Theta.t) (plaintyp : plaintyp) : plaintyp =
  match plaintyp.it with
  | BoolT | NumT _ | TextT -> plaintyp
  | VarT (tid, targs) -> (
      match Theta.find_opt tid theta with
      | Some plaintyp ->
          (* (TODO) Support higher-order substitution *)
          assert (targs = []);
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

and subst_plaintyps (theta : Theta.t) (plaintyps : plaintyp list) :
    plaintyp list =
  List.map (subst_plaintyp theta) plaintyps

and subst_nottyp (theta : Theta.t) (nottyp : nottyp) : nottyp =
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

and subst_nottyps (theta : Theta.t) (nottyps : nottyp list) : nottyp list =
  List.map (subst_nottyp theta) nottyps

and subst_param (theta : Theta.t) (param : param) : param =
  match param.it with
  | ExpP plaintyp ->
      let plaintyp = subst_plaintyp theta plaintyp in
      ExpP plaintyp $ param.at
  | DefP _ -> param

and subst_params (theta : Theta.t) (params : param list) : param list =
  List.map (subst_param theta) params

and subst_targ (theta : Theta.t) (targ : targ) : targ =
  subst_plaintyp theta targ

and subst_targs (theta : Theta.t) (targs : targ list) : targ list =
  List.map (subst_targ theta) targs
