type t = Info.t t'
and 'a t' = BareName of Text.t | QualifiedName of (Text.t list * Text.t)

let tags (t : 'a t') : 'a =
  match t with
  | BareName name -> Text.tags name
  | QualifiedName (prefix, name) ->
      let infos = List.map Text.tags prefix in
      List.fold_right Info.merge infos (Text.tags name)
