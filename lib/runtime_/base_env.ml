open Sem

module Scope = MakeScope

module TDEnv = MakeEnv (Type)
module VEnv = MakeEnv (Value)
module TEnv = MakeEnv (Type)
