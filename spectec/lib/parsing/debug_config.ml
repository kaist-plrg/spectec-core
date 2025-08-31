(* Control level of parser output through environment variables *)

type debug_level = Quiet | Basic | Verbose | Full

let parse_debug_level = function
  | "quiet" -> Quiet
  | "basic" -> Basic
  | "verbose" -> Verbose
  | "full" -> Full
  | _ -> Quiet

let get_env_debug_level env_var =
  match Sys.getenv_opt env_var with
  | Some s -> parse_debug_level (String.lowercase_ascii s)
  | None -> Quiet

let debug_enabled current required =
  match (current, required) with
  | Quiet, _ -> false
  | Basic, Basic | Verbose, Basic | Full, Basic -> true
  | Verbose, Verbose | Full, Verbose -> true
  | Full, Full -> true
  | _ -> false

let lexer_debug_enabled level =
  let lvl = get_env_debug_level "P4SPEC_LEXER_DEBUG" in
  debug_enabled lvl level

let parser_debug_enabled level =
  let lvl = get_env_debug_level "P4SPEC_PARSER_DEBUG" in
  debug_enabled lvl level

let context_debug_enabled level =
  let lvl = get_env_debug_level "P4SPEC_CONTEXT_DEBUG" in
  debug_enabled lvl level

let context_debug_print fmt =
  if context_debug_enabled Basic then Printf.printf fmt
  else Printf.ifprintf stdout fmt

let get_lexer_debug_level () = get_env_debug_level "P4SPEC_LEXER_DEBUG"
let get_parser_debug_level () = get_env_debug_level "P4SPEC_PARSER_DEBUG"
let get_context_debug_level () = get_env_debug_level "P4SPEC_CONTEXT_DEBUG"
