open Il.Ast
module Types = Runtime_type.Types
open Util.Source

let postprocess_mthd' mthd' =
  match mthd' with
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
      ExternConsM { id; tparams_hidden = []; cparams; annos }
  | _ -> mthd'

let postprocess_decl' decl' =
  match decl' with
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
      PackageTypeD { id; tparams; tparams_hidden = []; cparams; annos }
  | _ -> decl'

let postprocess_program (_program : Il.Ast.program) : Il.Ast.program =
  failwith "Not implemented"
