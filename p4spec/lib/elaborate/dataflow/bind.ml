open Domain.Lib
open Il.Ast
open Runtime_static
open Error
open Envs
open Util.Source

(* Binding occurrences of identifiers, singular or multiple (parallel) *)

module Occ = struct
  type t = Single of Typ.t | Multi of Typ.t

  let strip = function Single typ -> typ | Multi typ -> typ
  let to_string t = t |> strip |> Typ.to_string

  let add_iter (iter : iter) = function
    | Single typ -> Single (Typ.add_iter iter typ)
    | Multi typ -> Multi (Typ.add_iter iter typ)
end

(* Environment for identifier bindings *)

module BEnv = struct
  include MakeIdEnv (Occ)

  let singleton id typ = add id (Occ.Single (typ, [])) empty
  let flatten (benv : t) : VEnv.t = map Occ.strip benv

  let union (benv_a : t) (benv_b : t) : t =
    let ids = IdSet.union (dom benv_a) (dom benv_b) in
    IdSet.fold
      (fun id benv ->
        let bind_a = find_opt id benv_a in
        let bind_b = find_opt id benv_b in
        match (bind_a, bind_b) with
        | Some bind_a, Some bind_b ->
            let typ_a = Occ.strip bind_a in
            let typ_b = Occ.strip bind_b in
            if not (Typ.equiv typ_a typ_b) then
              error id.at
                (Format.asprintf
                   "inconsistent dimensions for multiple bindings: (left) %s, \
                    (right) %s"
                   (Occ.to_string bind_a) (Occ.to_string bind_b));
            add id (Occ.Multi typ_a) benv
        | Some bind, None | None, Some bind -> add id bind benv
        | None, None -> assert false)
      ids empty
end
