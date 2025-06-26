(* Parser debugging utilities using Menhir's inspection API *)

module MI = MenhirLib.General
module I = Parser.Incremental
module Engine = Parser.MenhirInterpreter

type debug_level = Quiet | Basic | Verbose | Full

let debug_level = ref Basic
let set_debug_level level = debug_level := level

let token_name token =
  try
    match token with
    | Parser.ABSTRACT _ -> "ABSTRACT"
    | Parser.ACTION _ -> "ACTION"
    | Parser.ACTIONS _ -> "ACTIONS"
    | Parser.APPLY _ -> "APPLY"
    | Parser.BOOL _ -> "BOOL"
    | Parser.BIT _ -> "BIT"
    | Parser.BREAK _ -> "BREAK"
    | Parser.CONST _ -> "CONST"
    | Parser.CONTINUE _ -> "CONTINUE"
    | Parser.CONTROL _ -> "CONTROL"
    | Parser.DEFAULT _ -> "DEFAULT"
    | Parser.ELSE _ -> "ELSE"
    | Parser.ENTRIES _ -> "ENTRIES"
    | Parser.ENUM _ -> "ENUM"
    | Parser.ERROR _ -> "ERROR"
    | Parser.EXIT _ -> "EXIT"
    | Parser.EXTERN _ -> "EXTERN"
    | Parser.HEADER _ -> "HEADER"
    | Parser.HEADER_UNION _ -> "HEADER_UNION"
    | Parser.IF _ -> "IF"
    | Parser.IN _ -> "IN"
    | Parser.INOUT _ -> "INOUT"
    | Parser.INT _ -> "INT"
    | Parser.KEY _ -> "KEY"
    | Parser.LIST _ -> "LIST"
    | Parser.SELECT _ -> "SELECT"
    | Parser.MATCH_KIND _ -> "MATCH_KIND"
    | Parser.OUT _ -> "OUT"
    | Parser.PACKAGE _ -> "PACKAGE"
    | Parser.PARSER _ -> "PARSER"
    | Parser.PRIORITY _ -> "PRIORITY"
    | Parser.RETURN _ -> "RETURN"
    | Parser.STATE _ -> "STATE"
    | Parser.STRING _ -> "STRING"
    | Parser.STRUCT _ -> "STRUCT"
    | Parser.SWITCH _ -> "SWITCH"
    | Parser.TABLE _ -> "TABLE"
    | Parser.THIS _ -> "THIS"
    | Parser.TRANSITION _ -> "TRANSITION"
    | Parser.TUPLE _ -> "TUPLE"
    | Parser.TYPEDEF _ -> "TYPEDEF"
    | Parser.TYPE _ -> "TYPE"
    | Parser.VALUESET _ -> "VALUESET"
    | Parser.VARBIT _ -> "VARBIT"
    | Parser.VOID _ -> "VOID"
    | Parser.TRUE _ -> "TRUE"
    | Parser.FALSE _ -> "FALSE"
    | Parser.END _ -> "END"
    | Parser.TYPENAME -> "TYPENAME"
    | Parser.IDENTIFIER -> "IDENTIFIER"
    | Parser.NAME _ -> "NAME"
    | Parser.STRING_LITERAL _ -> "STRING_LITERAL"
    | Parser.NUMBER _ -> "NUMBER"
    | Parser.LE _ -> "LE"
    | Parser.GE _ -> "GE"
    | Parser.SHL _ -> "SHL"
    | Parser.AND _ -> "AND"
    | Parser.OR _ -> "OR"
    | Parser.NE _ -> "NE"
    | Parser.EQ _ -> "EQ"
    | Parser.PLUS _ -> "PLUS"
    | Parser.MINUS _ -> "MINUS"
    | Parser.PLUS_SAT _ -> "PLUS_SAT"
    | Parser.MINUS_SAT _ -> "MINUS_SAT"
    | Parser.MUL _ -> "MUL"
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
  with _ -> "UNKNOWN_TOKEN"

let state_description state_num =
  match state_num with
  | 1 -> "void"
  | 2 -> "typedef"
  | 3 -> "tuple"
  | 4 -> "tuple <"
  | _ -> "unknown"

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

  match states with
  | [] ->
      if !debug_level >= Basic then
        Printf.printf "Parser: Current state: %d (%s)\n" current_state
          (state_description current_state);
      if !debug_level >= Verbose then
        Printf.printf "Parser: No stack elements\n"
  | _ ->
      if !debug_level >= Basic then
        Printf.printf "Parser: Current state: %d (%s)\n" current_state
          (state_description current_state);
      if !debug_level >= Verbose then
        Printf.printf "Parser: Stack: [%s]\n"
          (String.concat "; " (List.map string_of_int states))

let debug_parse lexer lexbuf =
  let supplier = Engine.lexer_lexbuf_to_supplier lexer lexbuf in
  let checkpoint = I.p4program lexbuf.lex_curr_p in
  let rec loop checkpoint =
    (match checkpoint with
    | Engine.InputNeeded env -> print_state env
    | Engine.Shifting (env, _, _) -> print_state env
    | Engine.AboutToReduce (env, _) ->
        print_state env;
        if !debug_level >= Verbose then
          Printf.printf "Parser: About to reduce\n"
    | Engine.HandlingError env ->
        print_state env;
        if !debug_level >= Basic then Printf.printf "Parser: Handling error\n"
    | _ -> ());
    match checkpoint with
    | Engine.InputNeeded _env ->
        let token, _, _ = supplier () in
        if !debug_level >= Verbose then
          Printf.printf "Parser: Consuming token: %s\n" (token_name token);
        loop
          (Engine.offer checkpoint (token, Lexing.dummy_pos, Lexing.dummy_pos))
    | Engine.Shifting _ | Engine.AboutToReduce _ ->
        loop (Engine.resume checkpoint)
    | Engine.HandlingError _env ->
        if !debug_level >= Basic then
          Printf.printf "Parser: Syntax error occurred\n";
        raise Parser.Error
    | Engine.Accepted v ->
        if !debug_level >= Basic then
          Printf.printf "Parser: Parsing completed successfully\n";
        v
    | Engine.Rejected -> failwith "Parser: Rejected"
  in
  if !debug_level >= Basic then
    Printf.printf "Parser: Starting parse with debug level %s\n"
      (match !debug_level with
      | Quiet -> "quiet"
      | Basic -> "basic"
      | Verbose -> "verbose"
      | Full -> "full");
  loop checkpoint
