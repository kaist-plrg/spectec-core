open Diagnostic
module Source = Common.Source

let ( let* ) = Result.bind

type spec_source = { filename : string; contents : string }

let with_lexbuf name lexbuf start =
  let open Lexing in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
  try start Lexer.token lexbuf
  with Parser.Error ->
    error ~code:Unexpected_token (Lexer.region lexbuf)
      "syntax error: unexpected token"

let rec map_in_order f = function
  | [] -> Ok []
  | x :: rest ->
      let* y = f x in
      let* ys = map_in_order f rest in
      Ok (y :: ys)

let parse_source { filename; contents } : Lang.El.spec result =
  try Ok (with_lexbuf filename (Lexing.from_string contents) Parser.spec)
  with ParseError e -> Error e

let parse_sources sources : Lang.El.spec result =
  let* specs = map_in_order parse_source sources in
  Ok (List.concat specs)

let read_source filename : spec_source result =
  try
    Ok
      {
        filename;
        contents = In_channel.with_open_bin filename In_channel.input_all;
      }
  with Sys_error msg ->
    Error (Diag.error ~source:"io" (Source.region_of_file filename) msg)

let parse_files filenames : Lang.El.spec result =
  let* sources = map_in_order read_source filenames in
  parse_sources sources

type error = Diagnostic.error
type 'a result = 'a Diagnostic.result

let error_to_diagnostic = Diagnostic.to_diagnostic
