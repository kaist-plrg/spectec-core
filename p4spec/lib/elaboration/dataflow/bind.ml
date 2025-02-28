open Domain.Lib
open Il.Ast
open Runtime_static
open Error
open Envs
module DCtx = Dctx
open Util.Source

(* Binding occurrences of identifiers, singular or multiple (parallel) *)

module Occ = struct
  type t = Single of typ' * Dim.t | Multi of typ' * Dim.t

  let to_string = function
    | Single (typ, dim) | Multi (typ, dim) ->
        Format.asprintf "(%s)%s"
          (Il.Print.string_of_typ (typ $ no_region))
          (Dim.to_string dim)

  let get_typ = function Single (typ, _) -> typ | Multi (typ, _) -> typ
  let get_dim = function Single (_, dim) -> dim | Multi (_, dim) -> dim

  let add_dim (iter : iter) = function
    | Single (typ, iters) -> Single (typ, iters @ [ iter ])
    | Multi (typ, iters) -> Multi (typ, iters @ [ iter ])
end

(* Environment for identifier bindings *)

module BEnv = struct
  include MakeIdEnv (Occ)

  let singleton id typ = add id (Occ.Single (typ, [])) empty
  let flatten (benv : t) : VEnv.t = map Occ.get_dim benv

  let union (benv_a : t) (benv_b : t) : t =
    let ids = IdSet.union (dom benv_a) (dom benv_b) in
    IdSet.fold
      (fun id benv ->
        let bind_a = find_opt id benv_a in
        let bind_b = find_opt id benv_b in
        match (bind_a, bind_b) with
        | Some bind_a, Some bind_b ->
            let typ_a, dim_a = (Occ.get_typ bind_a, Occ.get_dim bind_a) in
            let _typ_b, dim_b = (Occ.get_typ bind_b, Occ.get_dim bind_b) in
            (* (TODO) Also check that types are equivalent *)
            if not (Dim.equiv dim_a dim_b) then
              error id.at
                (Format.asprintf
                   "inconsistent dimensions for multiple bindings of %s: \
                    (left) %s, (right) %s"
                   (Id.to_string id) (Occ.to_string bind_a)
                   (Occ.to_string bind_b));
            add id (Occ.Multi (typ_a, dim_a)) benv
        | Some bind, None | None, Some bind -> add id bind benv
        | None, None -> assert false)
      ids empty
end
