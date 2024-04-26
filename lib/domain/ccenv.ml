open Ds

type t = Cclos.t Env.t

let pp fmt ienv =
  let bindings =
    Env.bindings ienv
    |> List.fold_left
         (fun acc (var, cclos) ->
           acc @ [ Format.asprintf "%s : %a" var Cclos.pp cclos ])
         []
  in
  Format.fprintf fmt "%s" ("{ " ^ String.concat ", " bindings ^ " }")
