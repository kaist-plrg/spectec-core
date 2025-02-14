open Domain.Dom

(* Environments *)

module TEnv = MakeIdEnv (Type)
module TDEnv = MakeTIdEnv (Typedef)
module REnv = MakeRIdEnv (Rel)
