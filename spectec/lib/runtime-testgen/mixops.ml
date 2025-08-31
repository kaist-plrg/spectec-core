open Xl

module Group = Set.Make (struct
  type t = Mixop.t

  let compare = compare
end)

module Family = Set.Make (struct
  type t = Group.t

  let compare = compare
end)

type t = Family.t

let to_string t =
  t |> Family.elements
  |> List.concat_map Group.elements
  |> List.map Mixop.string_of_mixop
  |> String.concat ", "
