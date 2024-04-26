open Ds

module TDEnv = struct
  type t = Type.t Env.t

  let pp fmt tdenv =
    let bindings =
      Env.bindings tdenv
      |> List.fold_left
           (fun acc (var, typ) ->
             acc @ [ Format.asprintf "%s : %a" var Type.pp typ ])
           []
    in
    Format.fprintf fmt "%s" ("{ " ^ String.concat ", " bindings ^ " }")
end

module Env = struct
  type t = Loc.t Env.t

  let pp fmt env =
    let bindings =
      Env.bindings env
      |> List.sort (fun (_, loc) (_, loc') -> Loc.compare loc loc')
      |> List.fold_left
           (fun acc (var, loc) ->
             acc @ [ Printf.sprintf "%s : %s" var (Loc.print loc) ])
           []
    in
    Format.fprintf fmt "%s" ("{ " ^ String.concat ", " bindings ^ " }")
end

module Sto = struct
  type tsto = Type.t Heap.t
  type vsto = Value.t Heap.t
  type t = tsto * vsto

  let pp fmt sto =
    let tsto, vsto = sto in
    let tsto_bindings =
      Heap.bindings tsto
      |> List.sort (fun (loc, _) (loc', _) -> Loc.compare loc loc')
      |> List.fold_left
           (fun acc (loc, typ) ->
             acc @ [ Format.asprintf "%s : %a" (Loc.print loc) Type.pp typ ])
           []
    in
    let vsto_bindings =
      Heap.bindings vsto
      |> List.sort (fun (loc, _) (loc', _) -> Loc.compare loc loc')
      |> List.fold_left
           (fun acc (loc, value) ->
             acc @ [ Format.asprintf "%s : %a" (Loc.print loc) Value.pp value ])
           []
    in
    Format.fprintf fmt "%s, %s"
      ("{ " ^ String.concat ", " tsto_bindings ^ " }")
      ("{ " ^ String.concat ", " vsto_bindings ^ " }")
end
