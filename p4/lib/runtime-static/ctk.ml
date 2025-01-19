module F = Format

type t = LCTK | CTK | DYN

let pp fmt t =
  match t with
  | LCTK -> F.fprintf fmt "local compile-time known"
  | CTK -> F.fprintf fmt "compile-time known"
  | DYN -> F.fprintf fmt "dynamic"

let eq ctk_a ctk_b =
  match (ctk_a, ctk_b) with
  | LCTK, LCTK -> true
  | CTK, CTK -> true
  | DYN, DYN -> true
  | _ -> false

let is_lctk = function LCTK -> true | _ -> false
let is_ctk = function LCTK | CTK -> true | _ -> false

let join ctk_a ctk_b =
  match (ctk_a, ctk_b) with
  | LCTK, LCTK -> LCTK
  | LCTK, CTK | CTK, LCTK | CTK, CTK -> CTK
  | _ -> DYN

let joins ts = List.fold_left join LCTK ts
