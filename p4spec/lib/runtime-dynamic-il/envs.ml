open Domain.Lib

(* Environments *)

(* Value environment *)

module VarMap = struct
  include Map.Make (Var)
end

module MakeVarEnv (V : sig
  type t

  val to_string : t -> string
end) =
struct
  include VarMap

  type t = V.t VarMap.t

  let to_string env =
    let to_string_binding (var, v) =
      Var.to_string var ^ " : " ^ V.to_string v
    in
    let bindings = bindings env in
    String.concat ", " (List.map to_string_binding bindings)
end

module VEnv = MakeVarEnv (Value)

(* Type definition environment *)

module TDEnv = MakeTIdEnv (Typdef)

(* Relation environment *)

module REnv = MakeRIdEnv (Rel)

(* Definition environment *)

module FEnv = MakeFIdEnv (Func)
