open Util.Error

let error = error_parse
let error_no_region = error_parse_no_region

let preprocess (includes : string list) (filename : string) =
  try Preprocessor.preprocess includes filename
  with _ -> "preprocessor error" |> error_no_region

let lex (filename : string) (file : string) =
  try
    let () = Lexer.reset () in
    let () = Lexer.set_filename filename in
    Lexer.enable_lexer_debug ();
    Lexing.from_string file
  with Lexer.Error s -> Format.asprintf "lexer error: %s" s |> error_no_region

let parse (lexbuf : Lexing.lexbuf) =
  try Parser.p4program Lexer.lexer lexbuf with
  | Parser.Error ->
      let info = Lexer.info lexbuf in
      "parser error" |> error (Source.to_region info)
  | Lexer.Error s ->
      Format.asprintf "lexer error: %s" s |> error_no_region
  | _ -> "unknown error" |> error_no_region

let parse_string (filename : string) (str : string) : Il.Ast.value =
  (* assume str is preprocessed *)
  let tokens = lex filename str in
  parse tokens

let parse_file (includes : string list) (filename : string) : Il.Ast.value =
  let program = preprocess includes filename in
  parse_string filename program

let parse_and_print_file (includes : string list) (filename : string) : string =
  let program = parse_file includes filename in
  Format.asprintf "%a\n" Pp.pp_value program

let roundtrip_file (includes : string list) (filename : string) : Il.Ast.value =
  let program = parse_file includes filename in
  let program_str = Format.asprintf "%a\n" Pp.pp_value program in
  let program' = parse_string filename program_str in
  (* if not (Il.Eq.eq_value program program') then *)
  (*   "roundtrip error" |> error_no_region; *)
  program'

