open Syntax
open Ast
open Utils
open Stackenv

(* Constructor closure environment *)

module CCEnv = StackEnv(Var)(Cclosure)
type t = CCEnv.t

let rec find_from_type (typ : Type.t) (ccenv : t) =
  match typ with
  | Type.TypeName { name = Name.BareName text; _ } -> (CCEnv.find text.str ccenv, [])
  | Type.TypeName { name = Name.QualifiedName ([], text); _ } ->
      (CCEnv.find_toplevel text.str ccenv, [])
  | Type.SpecializedType { base; args; _ } ->
      let cclos, _ = find_from_type base ccenv in
      (cclos, args)
  | _ ->
      Printf.sprintf "Constructor closure %s not found" (Pretty.print_type typ)
      |> failwith
