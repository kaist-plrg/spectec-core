module F = Format

type t = No of [ `CTK | `LCTK | `DYN ] | In | Out | InOut

let pp fmt dir =
  match dir with
  | No `CTK -> F.pp_print_string fmt "static"
  | No `LCTK -> F.pp_print_string fmt "local-static"
  | No `DYN -> F.pp_print_string fmt "dynamic"
  | In -> F.pp_print_string fmt "in"
  | Out -> F.pp_print_string fmt "out"
  | InOut -> F.pp_print_string fmt "inout"
