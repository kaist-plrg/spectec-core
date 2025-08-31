open P4util.Error

let error_info = error_parser_info
let error_no_info = error_parser_no_info

let preprocess (includes : string list) (filename : string) =
  try Preprocessor.preprocess includes filename
  with _ -> "preprocessor error" |> error_no_info

let lex (filename : string) (file : string) =
  try
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Lexing.from_string file
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error_no_info

let parse (lexbuf : Lexing.lexbuf) =
  try Parser.p4program Lexer.lexer lexbuf |> Transform.transform_program with
  | Parser.Error ->
      let info = Lexer.info lexbuf in
      "parser error" |> error_info info
  | _ -> "transform error" |> error_no_info

let parse_file (includes : string list) (filename : string) =
  let file = preprocess includes filename in
  let tokens = lex filename file in
  parse tokens

let parse_string (filename : string) (str : string) =
  (* assume str is preprocessed *)
  let tokens = lex filename str in
  parse tokens

let roundtrip_file (includes : string list) (filename : string) =
  let program = parse_file includes filename in
  let program_str = Format.asprintf "%a\n" P4el.Pp.pp_program program in
  let program' = parse_string filename program_str in
  if not (P4el.Eq.eq_program program program') then
    "roundtrip error" |> error_no_info;
  program
