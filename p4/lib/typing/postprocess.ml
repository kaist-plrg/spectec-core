module W = Lang.Walk_transform
open Il.Ast
module Types = Runtime_type.Types
open Util.Source

let base_walker = Il.Walk_transform.walker

type walker = Il.Walk_transform.walker

let hide_tparams_mthd (walker : walker) mthd =
  match mthd.it with
  | ExternConsM { id; tparams_hidden; cparams; annos } ->
      let theta =
        tparams_hidden
        |> List.map (fun tparam -> (tparam.it, Types.AnyT))
        |> Domain.Dom.TIdMap.of_list
      in
      let cparams =
        cparams
        |> List.map (fun cparam ->
               Runtime_type.Subst.subst_cparam theta cparam.it $ cparam.at)
      in
      let it = ExternConsM { id; tparams_hidden = []; cparams; annos } in
      { mthd with it }
  | _ -> base_walker.walk_mthd walker mthd

let hide_tparams_decl (walker : walker) decl =
  match decl.it with
  | PackageTypeD { id; tparams; tparams_hidden; cparams; annos } ->
      let theta =
        tparams_hidden
        |> List.map (fun tparam -> (tparam.it, Types.AnyT))
        |> Domain.Dom.TIdMap.of_list
      in
      let cparams =
        cparams
        |> List.map (fun cparam ->
               Runtime_type.Subst.subst_cparam theta cparam.it $ cparam.at)
      in
      let it =
        PackageTypeD { id; tparams; tparams_hidden = []; cparams; annos }
      in
      { decl with it }
  | _ -> base_walker.walk_decl walker decl

let hide_tparams_typ (walker : walker) typ =
  let walk_typ' = W.walk_it (walker.walk_typ walker) () in
  match typ.it with
  | SpecT (tdp, typs) ->
      let tdp =
        let tparams, tparams_hidden, typ = tdp in
        let theta =
          tparams_hidden
          |> List.map (fun tparam -> (tparam, Types.AnyT))
          |> Domain.Dom.TIdMap.of_list
        in
        let typ = Runtime_type.Subst.subst_typ theta typ in
        (tparams, [], typ)
      in
      let it = SpecT (tdp, W.walk_list walk_typ' typs) in
      { typ with it }
  | _ -> base_walker.walk_typ walker typ

let postprocess_program (program : Il.Ast.program) : Il.Ast.program =
  let hide_tparams =
    {
      Il.Walk_transform.walker with
      walk_mthd = hide_tparams_mthd;
      walk_decl = hide_tparams_decl;
      walk_typ = hide_tparams_typ;
    }
  in
  hide_tparams.walk_program hide_tparams program
