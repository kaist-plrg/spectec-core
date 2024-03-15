type t = string list

module PMap = Map.Make(
  struct
    type t = string list
    let compare = compare
  end
)

let print (path: t) = String.concat "." path
