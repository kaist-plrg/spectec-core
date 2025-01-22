module F = Format
open Util.Error

let error_no_info = error_interp_no_info

let gen arch =
  match arch with
  | "v1model" ->
      (module Driver.Make (V1model.Make) (Interp.Make) : Driver.DRIVER)
  | "ebpf" -> (module Driver.Make (Ebpf.Make) (Interp.Make) : Driver.DRIVER)
  | _ ->
      F.asprintf "(gen) architecture %s is not supported" arch |> error_no_info
