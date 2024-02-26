open P4cherry

let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let chan = open_in filename in

  let lexbuf = Lexing.from_channel chan in
  let _ast =
    try Parser.p4program Lexer.lexer lexbuf with
    | Lexer.Error msg ->
        Printf.sprintf "Lexer error: %s\n" msg |> failwith
    | Parser.Error ->
        let info = Lexer.info lexbuf in
        Info.to_string info |> Printf.sprintf "Parser error at %s\n" |> failwith
    | _ ->
        failwith "?"
  in

  close_in chan;

  print_endline "Parsing success. (TODO: print AST)"
