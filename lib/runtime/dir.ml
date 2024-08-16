module F = Format

type t = In | Out | InOut | No of [ `LCTK | `CTK | `DYN ]

let pp fmt dir =
  match dir with
  | In -> F.pp_print_string fmt "in"
  | Out -> F.pp_print_string fmt "out"
  | InOut -> F.pp_print_string fmt "inout"
  | No `LCTK -> F.pp_print_string fmt "local-static"
  | No `CTK -> F.pp_print_string fmt "static"
  | No `DYN -> F.pp_print_string fmt "dynamic"
