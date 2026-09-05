let parse_file includes filename =
  try Parse.parse_file includes filename |> Result.ok
  with Error.P4ParseError (at, msg) ->
    Spectec.Error.TaskParseError (at, msg) |> Result.error

let unparse ~spec:spec_il value_program =
  match value_program with
  | [ v ] -> Format.asprintf "%a\n" (Concrete.Pp.pp_program spec_il) v
  | _ -> failwith "unexpected number of values"

let parse_string ~spec:_ ~filename content =
  Parse.parse_string filename content |> fun v -> Ok [ v ]
