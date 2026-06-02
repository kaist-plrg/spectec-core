open Diagnostic
module Source = Common.Source

let with_lexbuf name lexbuf start =
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
  try start Lexer.token lexbuf
  with Parser.Error ->
    error ~code:Unexpected_token (Lexer.region lexbuf)
      "syntax error: unexpected token"

let parse_file file : Lang.El.spec result =
  try
    let ic = open_in file in
    let spec =
      Fun.protect
        (fun () -> with_lexbuf file (Lexing.from_channel ic) Parser.spec)
        ~finally:(fun () -> close_in ic)
    in
    Ok spec
  with
  | ParseError e -> Error e
  | Sys_error msg ->
      Error (Diag.error ~source:"io" (Source.region_of_file file) msg)

let parse_string ~origin source : Lang.El.spec result =
  try Ok (with_lexbuf origin (Lexing.from_string source) Parser.spec)
  with ParseError e -> Error e

let parse_files filenames : Lang.El.spec result =
  let rec parse_files' acc = function
    | [] -> Ok (List.concat (List.rev acc))
    | file :: rest -> (
        match parse_file file with
        | Ok spec -> parse_files' (spec :: acc) rest
        | Error e -> Error e)
  in
  parse_files' [] filenames

type error = Diagnostic.error
type 'a result = 'a Diagnostic.result

let error_to_diagnostic = Diagnostic.to_diagnostic
