type t = string list
type key = t

module Path = struct
  type t = key 
  let compare = compare
end

module PMap = Map.Make(Path)
