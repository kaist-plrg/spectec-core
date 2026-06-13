(** [${syntax: ids}] — collects every EL syntax definition, keyed by type id. *)

open Lang
open Common.Source

let key_of_def (def : El.def) : (string * El.def) option =
  match def.it with TypD (id, _, _, _) -> Some (id.it, def) | _ -> None

let extract (spec_el : El.spec) : (string * string) list =
  spec_el |> List.filter_map key_of_def
  |> List.map (fun (key, def) -> (key, El.Print.string_of_def def))

let source : Anchor.Source.entry =
  {
    frame =
      { name = "syntax"; prefix = "----\n"; suffix = "\n----"; header = false };
    extract;
  }
