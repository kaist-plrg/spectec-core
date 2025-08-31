open Il.Ast
open Util.Source
module SMap = Map.Make (String)

type hmap = (nottyp * El.Ast.exp) list SMap.t

let hintid = "print"

(* Types *)

let hints_of_typcase hmap (id : id) (typcase : typcase) : hmap =
  let nottyp, hints = typcase in
  match List.find_opt (fun hint -> hint.hintid.it = hintid) hints with
  | Some hint ->
      let hint = (nottyp, hint.hintexp) in
      let add = function
        | None -> Some [ hint ]
        | Some hints -> Some (hints @ [ hint ])
      in
      SMap.update id.it add hmap
  | None -> hmap

let hints_of_typcases hmap (id : id) (typcases : typcase list) : hmap =
  List.fold_left
    (fun hmap typcase -> hints_of_typcase hmap id typcase)
    hmap typcases

let hints_of_deftyp hmap (id : id) (deftyp : deftyp) : hmap =
  match deftyp.it with
  | VariantT typcases -> hints_of_typcases hmap id typcases
  | _ -> hmap

(* Definitions *)

let hints_of_def hmap (def : def) : hmap =
  match def.it with
  | TypD (id, _, deftyp) -> hints_of_deftyp hmap id deftyp
  | _ -> hmap

(* Spec *)

let hints_of_spec (spec : spec) : hmap =
  List.fold_left (fun hmap def -> hints_of_def hmap def) SMap.empty spec
