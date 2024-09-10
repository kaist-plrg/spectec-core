module L = Lang.Ast
module P = Lang.Pp
module F = Format

type t = L.dir'

let pp fmt t = F.fprintf fmt "%a" P.pp_dir' t

(*type tt = In | Out | InOut | No [ `LCTK | `CTK of Value.t ]*)
