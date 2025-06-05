open Domain.Lib

(* Environments *)

(* Value environment *)

module VEnv = Runtime_dynamic.Envs.VEnv

(* Type definition environment *)

module TDEnv = Runtime_dynamic.Envs.TDEnv

(* Relation environment *)

module REnv = MakeRIdEnv (Rel)

(* Definition environment *)

module FEnv = MakeFIdEnv (Func)
