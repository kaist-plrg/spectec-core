open Ds

type t = Object.t Env.t

let pp fmt ienv =
  let bindings =
    Env.bindings ienv
    |> List.fold_left
         (fun acc (var, obj) ->
           acc @ [ Format.asprintf "%s : %a" var Object.pp obj ])
         []
  in
  Format.fprintf fmt "%s" ("{ " ^ String.concat ", " bindings ^ " }")
