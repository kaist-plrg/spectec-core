open Common.Source
module El = Lang.El

type t = (string * region) list

let empty = []
let add name region acc = (name, region) :: acc

let atom_use (atom : El.atom) acc =
  match Index.atom_name atom with
  | Some name -> add name atom.at acc
  | None -> acc

let rec plaintyp_uses (plaintyp : El.plaintyp) acc =
  match plaintyp.it with
  | El.BoolT | El.NumT _ | El.TextT -> acc
  | El.VarT (id, targs) ->
      List.fold_left
        (fun acc targ -> plaintyp_uses targ acc)
        (add id.it id.at acc) targs
  | El.ParenT plaintyp -> plaintyp_uses plaintyp acc
  | El.TupleT plaintyps ->
      List.fold_left (fun acc p -> plaintyp_uses p acc) acc plaintyps
  | El.IterT (plaintyp, _) -> plaintyp_uses plaintyp acc

and typ_uses (typ : El.typ) acc =
  match typ with
  | El.PlainT plaintyp -> plaintyp_uses plaintyp acc
  | El.NotationT nottyp -> nottyp_uses nottyp acc

and nottyp_uses (nottyp : El.nottyp) acc =
  match nottyp.it with
  | El.AtomT atom -> atom_use atom acc
  | El.SeqT typs -> List.fold_left (fun acc t -> typ_uses t acc) acc typs
  | El.InfixT (typ_l, atom, typ_r) ->
      typ_uses typ_r (atom_use atom (typ_uses typ_l acc))
  | El.BrackT (atom_l, typ, atom_r) ->
      atom_use atom_r (typ_uses typ (atom_use atom_l acc))

(* Case/field atoms declare names; their types reference them. *)
and deftyp_uses (deftyp : El.deftyp) acc =
  match deftyp.it with
  | El.PlainTD plaintyp -> plaintyp_uses plaintyp acc
  | El.StructTD typfields ->
      List.fold_left
        (fun acc (_atom, plaintyp, _hints) -> plaintyp_uses plaintyp acc)
        acc typfields
  | El.VariantTD typcases ->
      List.fold_left
        (fun acc (typ, _hints) ->
          match typ with
          | El.NotationT { it = El.SeqT (El.NotationT head :: rest); _ }
            when match head.it with El.AtomT _ -> true | _ -> false ->
              List.fold_left (fun acc t -> typ_uses t acc) acc rest
          | El.NotationT { it = El.AtomT _; _ } -> acc
          | typ -> typ_uses typ acc)
        acc typcases

and exp_uses (exp : El.exp) acc =
  match exp.it with
  | El.BoolE _ | El.NumE _ | El.TextE _ | El.EpsE | El.HoleE _ | El.LatexE _ ->
      acc
  | El.VarE id -> add id.it id.at acc
  | El.CallE (id, targs, args) ->
      let acc = add ("$" ^ id.it) id.at acc in
      let acc =
        List.fold_left (fun acc targ -> plaintyp_uses targ acc) acc targs
      in
      List.fold_left (fun acc arg -> arg_uses arg acc) acc args
  | El.AtomE atom -> atom_use atom acc
  | El.UnE (_, exp)
  | El.ArithE exp
  | El.LenE exp
  | El.ParenE exp
  | El.IterE (exp, _)
  | El.UnparenE exp ->
      exp_uses exp acc
  | El.BinE (exp_l, _, exp_r)
  | El.CmpE (exp_l, _, exp_r)
  | El.ConsE (exp_l, exp_r)
  | El.CatE (exp_l, exp_r)
  | El.IdxE (exp_l, exp_r)
  | El.MemE (exp_l, exp_r)
  | El.FuseE (exp_l, exp_r) ->
      exp_uses exp_r (exp_uses exp_l acc)
  | El.SliceE (exp_1, exp_2, exp_3) ->
      exp_uses exp_3 (exp_uses exp_2 (exp_uses exp_1 acc))
  | El.ListE exps | El.TupleE exps | El.SeqE exps ->
      List.fold_left (fun acc e -> exp_uses e acc) acc exps
  | El.StrE fields ->
      List.fold_left
        (fun acc (atom, exp) -> exp_uses exp (atom_use atom acc))
        acc fields
  | El.DotE (exp, atom) -> atom_use atom (exp_uses exp acc)
  | El.UpdE (exp_l, path, exp_r) ->
      exp_uses exp_r (path_uses path (exp_uses exp_l acc))
  | El.SubE (exp, plaintyp) -> plaintyp_uses plaintyp (exp_uses exp acc)
  | El.InfixE (exp_l, atom, exp_r) ->
      exp_uses exp_r (atom_use atom (exp_uses exp_l acc))
  | El.BrackE (atom_l, exp, atom_r) ->
      atom_use atom_r (exp_uses exp (atom_use atom_l acc))

and path_uses (path : El.path) acc =
  match path.it with
  | El.RootP -> acc
  | El.IdxP (path, exp) -> exp_uses exp (path_uses path acc)
  | El.SliceP (path, exp_1, exp_2) ->
      exp_uses exp_2 (exp_uses exp_1 (path_uses path acc))
  | El.DotP (path, atom) -> atom_use atom (path_uses path acc)

and arg_uses (arg : El.arg) acc =
  match arg.it with
  | El.ExpA exp -> exp_uses exp acc
  | El.DefA id -> add ("$" ^ id.it) id.at acc

and param_uses (param : El.param) acc =
  match param.it with
  | El.ExpP plaintyp -> plaintyp_uses plaintyp acc
  | El.DefP (id, _tparams, params, plaintyp) ->
      let acc = add ("$" ^ id.it) id.at acc in
      plaintyp_uses plaintyp
        (List.fold_left (fun acc p -> param_uses p acc) acc params)

and prem_uses (prem : El.prem) acc =
  match prem.it with
  | El.VarPr (id, plaintyp) -> plaintyp_uses plaintyp (add id.it id.at acc)
  | El.RulePr (id, exp) | El.RuleNotPr (id, exp) ->
      exp_uses exp (add id.it id.at acc)
  | El.IfPr exp | El.DebugPr exp -> exp_uses exp acc
  | El.ElsePr -> acc
  | El.IterPr (prem, _) -> prem_uses prem acc

let uses_of_def (def : El.def) acc =
  match def.it with
  (* Index records declarations; collect only their references here. *)
  | El.SynD _ | El.SepD -> acc
  | El.TypD (_id, _tparams, deftyp, _) -> deftyp_uses deftyp acc
  | El.VarD (_id, plaintyp, _) -> plaintyp_uses plaintyp acc
  | El.RelD (_id, nottyp, _) -> nottyp_uses nottyp acc
  | El.RuleD (relid, _ruleid, exp, prems) ->
      let acc = add relid.it relid.at acc in
      List.fold_left (fun acc p -> prem_uses p acc) (exp_uses exp acc) prems
  | El.DecD (_id, _tparams, params, plaintyp, _)
  | El.BuiltinDecD (_id, _tparams, params, plaintyp, _) ->
      plaintyp_uses plaintyp
        (List.fold_left (fun acc p -> param_uses p acc) acc params)
  | El.DefD (id, _tparams, args, exp, prems) ->
      let acc = add ("$" ^ id.it) id.at acc in
      let acc = List.fold_left (fun acc a -> arg_uses a acc) acc args in
      List.fold_left (fun acc p -> prem_uses p acc) (exp_uses exp acc) prems

let of_spec (spec : El.spec) : t =
  List.rev (List.fold_left (fun acc def -> uses_of_def def acc) empty spec)

let find (uses : t) name =
  let matches candidate =
    String.equal candidate name || String.equal (Index.base_name candidate) name
  in
  uses |> List.filter (fun (candidate, _) -> matches candidate) |> List.map snd
