(*
 * Parser debugging utilities using Menhir's inspection API:
 *
 *   Debugs parser stack state and token consumption
 *)

module MI = MenhirLib.General
module I = Parser.Incremental
module Engine = Parser.MenhirInterpreter
module P = Printf

let get_debug_level () = Debug_config.get_parser_debug_level ()

let token_name token =
  try
    match token with
    | Parser.ABSTRACT _ -> "abstract"
    | Parser.ACTION _ -> "action"
    | Parser.ACTIONS _ -> "actions"
    | Parser.APPLY _ -> "apply"
    | Parser.BOOL _ -> "bool"
    | Parser.BIT _ -> "bit"
    | Parser.BREAK _ -> "break"
    | Parser.CONST _ -> "const"
    | Parser.CONTINUE _ -> "continue"
    | Parser.CONTROL _ -> "control"
    | Parser.DEFAULT _ -> "default"
    | Parser.ELSE _ -> "else"
    | Parser.ENTRIES _ -> "entries"
    | Parser.ENUM _ -> "enum"
    | Parser.ERROR _ -> "error"
    | Parser.EXIT _ -> "exit"
    | Parser.EXTERN _ -> "extern"
    | Parser.HEADER _ -> "header"
    | Parser.HEADER_UNION _ -> "header_union"
    | Parser.IF _ -> "if"
    | Parser.IN _ -> "in"
    | Parser.INOUT _ -> "inout"
    | Parser.INT _ -> "int"
    | Parser.KEY _ -> "key"
    | Parser.LIST _ -> "list"
    | Parser.SELECT _ -> "select"
    | Parser.MATCH_KIND _ -> "match_kind"
    | Parser.OUT _ -> "out"
    | Parser.PACKAGE _ -> "package"
    | Parser.PARSER _ -> "parser"
    | Parser.PRIORITY _ -> "priority"
    | Parser.RETURN _ -> "return"
    | Parser.STATE _ -> "state"
    | Parser.STRING _ -> "string"
    | Parser.STRUCT _ -> "struct"
    | Parser.SWITCH _ -> "switch"
    | Parser.TABLE _ -> "table"
    | Parser.THIS _ -> "this"
    | Parser.TRANSITION _ -> "transition"
    | Parser.TUPLE _ -> "tuple"
    | Parser.TYPEDEF _ -> "typedef"
    | Parser.TYPE _ -> "type"
    | Parser.VALUE_SET _ -> "value_set"
    | Parser.VARBIT _ -> "varbit"
    | Parser.VOID _ -> "void"
    | Parser.TRUE _ -> "true"
    | Parser.FALSE _ -> "false"
    | Parser.FOR _ -> "for"
    | Parser.END _ -> "end"
    | Parser.TYPENAME -> "typename"
    | Parser.IDENTIFIER -> "identifier"
    | Parser.NAME s -> P.sprintf "name %s" (Il.Print.string_of_value s)
    | Parser.STRING_LITERAL _ -> "string_literal"
    | Parser.NUMBER _ -> "number"
    | Parser.LE _ -> "<="
    | Parser.GE _ -> ">="
    | Parser.SHL _ -> ">>"
    | Parser.AND _ -> "&"
    | Parser.OR _ -> "|"
    | Parser.NE _ -> "!="
    | Parser.EQ _ -> "=="
    | Parser.PLUS _ -> "+"
    | Parser.MINUS _ -> "-"
    | Parser.PLUS_SAT _ -> "PLUS_SAT"
    | Parser.MINUS_SAT _ -> "MINUS_SAT"
    | Parser.MUL _ -> "*"
    | Parser.INVALID _ -> "INVALID"
    | Parser.DIV _ -> "DIV"
    | Parser.MOD _ -> "MOD"
    | Parser.BIT_OR _ -> "BIT_OR"
    | Parser.BIT_AND _ -> "BIT_AND"
    | Parser.BIT_XOR _ -> "BIT_XOR"
    | Parser.COMPLEMENT _ -> "COMPLEMENT"
    | Parser.L_BRACKET _ -> "L_BRACKET"
    | Parser.R_BRACKET _ -> "R_BRACKET"
    | Parser.L_BRACE _ -> "L_BRACE"
    | Parser.R_BRACE _ -> "R_BRACE"
    | Parser.L_ANGLE _ -> "L_ANGLE"
    | Parser.L_ANGLE_ARGS _ -> "L_ANGLE_ARGS"
    | Parser.R_ANGLE _ -> "R_ANGLE"
    | Parser.R_ANGLE_SHIFT _ -> "R_ANGLE_SHIFT"
    | Parser.L_PAREN _ -> "L_PAREN"
    | Parser.R_PAREN _ -> "R_PAREN"
    | Parser.ASSIGN _ -> "ASSIGN"
    | Parser.COLON _ -> "COLON"
    | Parser.COMMA _ -> "COMMA"
    | Parser.QUESTION _ -> "QUESTION"
    | Parser.DOT _ -> "DOT"
    | Parser.NOT _ -> "NOT"
    | Parser.SEMICOLON _ -> "SEMICOLON"
    | Parser.AT _ -> "AT"
    | Parser.PLUSPLUS _ -> "PLUSPLUS"
    | Parser.DONTCARE _ -> "DONTCARE"
    | Parser.MASK _ -> "MASK"
    | Parser.DOTS _ -> "DOTS"
    | Parser.RANGE _ -> "RANGE"
    | Parser.PRAGMA _ -> "PRAGMA"
    | Parser.PRAGMA_END _ -> "PRAGMA_END"
    | Parser.UNEXPECTED_TOKEN _ -> "UNEXPECTED_TOKEN"
    | _ -> "unknown"
  with _ -> "UNKNOWN_TOKEN"

(* Recursively collect stack states using top and pop *)
let rec collect_stack env acc =
  match Parser.MenhirInterpreter.top env with
  | None -> List.rev acc
  | Some (Parser.MenhirInterpreter.Element (state, _, _, _)) -> (
      let state_num = Parser.MenhirInterpreter.number state in
      match Parser.MenhirInterpreter.pop env with
      | None -> List.rev (state_num :: acc)
      | Some env' -> collect_stack env' (state_num :: acc))

let print_state env =
  let current_state = Parser.MenhirInterpreter.current_state_number env in
  let states = collect_stack env [] in
  let debug_level = get_debug_level () in

  if Debug_config.debug_enabled debug_level Basic then
    Printf.printf "@State %d:\n" current_state;
  match states with
  | [] ->
      if Debug_config.debug_enabled debug_level Verbose then
        Printf.printf "+Stack empty\n"
  | _ ->
      if Debug_config.debug_enabled debug_level Verbose then
        Printf.printf "+Stack: [%s]\n"
          (String.concat "; " (List.map string_of_int states))

let debug_parse lexer lexbuf =
  let supplier = Engine.lexer_lexbuf_to_supplier lexer lexbuf in
  let checkpoint = I.p4program lexbuf.lex_curr_p in
  let debug_level = get_debug_level () in
  let rec loop checkpoint =
    (match checkpoint with
    | Engine.InputNeeded env -> print_state env
    | Engine.Shifting (env, _, _) -> print_state env
    | Engine.AboutToReduce (env, _) ->
        print_state env;
        if Debug_config.debug_enabled debug_level Verbose then
          Printf.printf "--- About to reduce\n"
    | Engine.HandlingError env ->
        print_state env;
        if Debug_config.debug_enabled debug_level Basic then
          Printf.printf "Parser: Handling error\n"
    | _ -> ());
    match checkpoint with
    | Engine.InputNeeded _env ->
        let token, _, _ = supplier () in
        if Debug_config.debug_enabled debug_level Verbose then
          Printf.printf "\n|-> Consuming token: %s\n\n" (token_name token);
        loop
          (Engine.offer checkpoint (token, Lexing.dummy_pos, Lexing.dummy_pos))
    | Engine.Shifting _ | Engine.AboutToReduce _ ->
        loop (Engine.resume checkpoint)
    | Engine.HandlingError _env ->
        if Debug_config.debug_enabled debug_level Basic then
          Printf.printf "Parser: Syntax error occurred\n";
        raise Parser.Error
    | Engine.Accepted v ->
        if Debug_config.debug_enabled debug_level Basic then
          Printf.printf "Parser: Parsing completed successfully\n";
        v
    | Engine.Rejected -> failwith "Parser: Rejected"
  in
  if Debug_config.debug_enabled debug_level Basic then
    Printf.printf "Parser: Starting parse with debug level %s\n"
      (match debug_level with
      | Quiet -> "quiet"
      | Basic -> "basic"
      | Verbose -> "verbose"
      | Full -> "full");
  loop checkpoint
