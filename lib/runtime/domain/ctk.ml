module F = Format

type t = LCTK | CTK | DYN

let pp fmt t =
  match t with
  | LCTK -> F.fprintf fmt "local compile-time known"
  | CTK -> F.fprintf fmt "compile-time known"
  | DYN -> F.fprintf fmt "dynamic"

let is_lctk = function LCTK -> true | _ -> false
let is_ctk = function LCTK | CTK -> true | _ -> false

let join t1 t2 =
  match (t1, t2) with
  | LCTK, LCTK -> LCTK
  | LCTK, CTK | CTK, LCTK | CTK, CTK -> CTK
  | _ -> DYN

let joins ts = List.fold_left join LCTK ts
