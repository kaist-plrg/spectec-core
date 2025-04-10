open Il.Ast
module Types = Runtime_type.Types
open Util.Source

let postprocess_mthd (walker : Il.Walk_transform.walker) mthd =
  let walk_mthd = Il.Walk_transform.walker.walk_mthd walker in
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
  | _ -> walk_mthd mthd

let postprocess_decl (walker : Il.Walk_transform.walker) decl =
  let walk_decl = Il.Walk_transform.walker.walk_decl walker in
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
  | _ -> walk_decl decl

let postprocess_program (program : Il.Ast.program) : Il.Ast.program =
  let walker =
    {
      Il.Walk_transform.walker with
      walk_mthd = postprocess_mthd;
      walk_decl = postprocess_decl;
    }
  in
  walker.walk_program walker program
