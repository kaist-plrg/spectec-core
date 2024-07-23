open Domain

(* Store maps object identifiers (fully-qualified paths) to objects *)
module Sto = MakeEnv (Path) (Object)
