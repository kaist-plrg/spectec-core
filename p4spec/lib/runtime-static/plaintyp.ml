open Domain.Lib
open El.Ast
open El.Print
open Util.Error
open Util.Source

(* Plain type *)

type t = plaintyp

let to_string t = string_of_plaintyp t

(* Conversion from IL type *)

let rec of_internal_typ (typ : Il.Ast.typ) : t =
  match typ.it with
  | BoolT -> BoolT $ typ.at
  | NumT numtyp -> NumT numtyp $ typ.at
  | TextT -> TextT $ typ.at
  | VarT (tid, targs) ->
      let targs = List.map of_internal_typ targs in
      VarT (tid, targs) $ typ.at
  | TupleT typs ->
      let typs = List.map of_internal_typ typs in
      TupleT typs $ typ.at
  | IterT (typ, Opt) ->
      let typ = of_internal_typ typ in
      IterT (typ, Opt) $ typ.at
  | IterT (typ, List) ->
      let typ = of_internal_typ typ in
      IterT (typ, List) $ typ.at
  | _ -> assert false

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
            error_elab plaintyp.at "higher-order substitution is disallowed";
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

(* Expansion of parentheses and type alaiases *)

type kind =
  (* Type that is not yet defined *)
  | Opaque
  (* Plain type *)
  | Plain of plaintyp
  (* Struct type *)
  | Struct of typfield list
  (* Variant type, with the second `plaintyp` field of each case being
     the type of each case, for subtyping purposes *)
  | Variant of (nottyp * plaintyp) list

type tdenv = Typdef.t TIdMap.t

let rec expand_plaintyp (tdenv : tdenv) (plaintyp : plaintyp) : plaintyp =
  match plaintyp.it with
  | VarT (tid, targs) -> (
      let td = TIdMap.find tid tdenv in
      match td with
      | Defined (tparams, typdef) -> (
          match typdef with
          | `Plain _ when List.length targs <> List.length tparams ->
              error_elab plaintyp.at "type arguments do not match"
          | `Plain plaintyp ->
              let theta = List.combine tparams targs |> TIdMap.of_list in
              let plaintyp = subst_plaintyp theta plaintyp in
              expand_plaintyp tdenv plaintyp
          | _ -> plaintyp)
      | _ -> plaintyp)
  | ParenT plaintyp -> expand_plaintyp tdenv plaintyp
  | _ -> plaintyp

let kind_plaintyp (tdenv : tdenv) (plaintyp : plaintyp) : kind =
  let plaintyp = expand_plaintyp tdenv plaintyp in
  match plaintyp.it with
  | VarT (tid, targs) -> (
      let td = TIdMap.find tid tdenv in
      match td with
      | Defined (tparams, typdef) -> (
          let theta = List.combine tparams targs |> TIdMap.of_list in
          match typdef with
          | `Plain plaintyp ->
              let plaintyp = subst_plaintyp theta plaintyp in
              Plain plaintyp
          | `Struct typfields ->
              let typfields =
                List.map
                  (fun (atom, plaintyp, hints) ->
                    let plaintyp = subst_plaintyp theta plaintyp in
                    (atom, plaintyp, hints))
                  typfields
              in
              Struct typfields
          | `Variant typcases ->
              let nottyps, plaintyps = List.split typcases in
              let nottyps = subst_nottyps theta nottyps in
              let plaintyps = subst_plaintyps theta plaintyps in
              let typcases = List.combine nottyps plaintyps in
              Variant typcases)
      | _ -> Opaque)
  | _ -> Plain plaintyp
