open Utils
open Flatmap
open Stackmap
open Stackset

(* Constant environment *)

module CEnv = StackSet (Var)

type cenv = CEnv.t

(* Local environment *)

module LEnv = StackSet (Var)

type lenv = LEnv.t

(* Value store *)

module VSto = StackMap (Var) (Value)

type vsto = VSto.t

(* Type environment *)

module TEnv = StackSet (Var)

type tenv = TEnv.t

(* Type Store *)

module TSto = StackMap (Var) (Typ)

type tsto = TSto.t

(* Type-alias environment *)

module TDEnv = FlatMap (Var) (Typ)

type tdenv = TDEnv.t

(* Loaders *)

let load_const (cenv : cenv) (tsto : tsto) (vsto : vsto) (name : string)
    (typ : Typ.t) (value : Value.t) : cenv * tsto * vsto =
  let cenv = CEnv.add name cenv in
  let tsto = TSto.add name typ tsto in
  let vsto = VSto.add name value vsto in
  (cenv, tsto, vsto)

let load_var (lenv : lenv) (tsto : tsto) (name : string) (typ : Typ.t) :
    lenv * tsto =
  let lenv = LEnv.add name lenv in
  let tsto = TSto.add name typ tsto in
  (lenv, tsto)

(* Finders *)

let find_const (cenv : cenv) (vsto : vsto) (name : string) : Value.t =
  let name = CEnv.find name cenv in
  VSto.find name vsto

let find_const_toplevel (cenv : cenv) (vsto : vsto) (name : string) : Value.t =
  let name = CEnv.find_toplevel name cenv in
  VSto.find_toplevel name vsto

let find_var (lenv : lenv) (vsto : vsto) (name : string) : Value.t =
  let name = LEnv.find name lenv in
  VSto.find name vsto

let find_var_toplevel (lenv : lenv) (vsto : vsto) (name : string) : Value.t =
  let name = LEnv.find_toplevel name lenv in
  VSto.find_toplevel name vsto

let find_value (cenv : cenv) (lenv : lenv) (vsto : vsto) (name : string) :
    Value.t =
  try find_const cenv vsto name with _ -> find_var lenv vsto name

let find_value_toplevel (cenv : cenv) (lenv : lenv) (vsto : vsto) (name : string) :
    Value.t =
  try find_const_toplevel cenv vsto name with _ -> find_var_toplevel lenv vsto name
