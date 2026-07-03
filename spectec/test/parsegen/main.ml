(* Differential test: Parsegen.Parse, driven only by the extracted grammar, must
   build the same IL value as impty's hand-written parser for every expression
   below. *)

open Spectec
module Parse = Parsegen.Parse
module Lexer = Targets_impty.Lexer
module Reference = Targets_impty.Parse

let filename = "<test>"

let load_grammar spec : Grammar.t =
  match parse_spec_files [ spec ] with
  | Ok el -> (
      match elaborate el with
      | Ok il -> Grammar.extract ~start:"expr" il
      | Error _ -> failwith "could not elaborate the impty spec")
  | Error _ -> failwith "could not parse the impty spec"

(* Maps each impty lexer token to the grammar terminal it stands for. *)
let classify : Lexer.token -> Grammar.Terminal.t option =
  let atom a = Some (Grammar.Terminal.Atom a) in
  let primitive kind value = Some (Grammar.Terminal.Primitive (kind, value)) in
  function
  | Lexer.Plus -> atom (Lang.Xl.Atom.operator "+")
  | Lexer.Leq -> atom (Lang.Xl.Atom.operator "<=")
  | Lexer.And -> atom (Lang.Xl.Atom.operator "&&")
  | Lexer.Bang -> atom (Lang.Xl.Atom.operator "!")
  | Lexer.Lparen -> atom Lang.Xl.Atom.LParen
  | Lexer.Rparen -> atom Lang.Xl.Atom.RParen
  | Lexer.Num n ->
      primitive (Grammar.Num `IntT) (Lang.Il.Value.int (Bigint.of_int n))
  | Lexer.KwTrue -> primitive Grammar.Bool (Lang.Il.Value.bool true)
  | Lexer.KwFalse -> primitive Grammar.Bool (Lang.Il.Value.bool false)
  | Lexer.Ident s -> primitive Grammar.Text (Lang.Il.Value.text s)
  | _ -> None

let lex (source : string) : Lexer.token list =
  Lexer.tokenize ~filename source
  |> List.filter_map (fun (token, _region) ->
         match token with Lexer.Eof -> None | token -> Some token)

let show_value = Lang.Il.Print_debug.string_of_value

let check (grammar : Grammar.t) (start : string)
    (parse_reference : filename:string -> string -> Lang.Il.value)
    (label : string) (source : string) : unit =
  let generated = Parse.run classify grammar start (lex source) in
  let reference = parse_reference ~filename source in
  let verdict =
    if Lang.Il.Eq.eq_value generated reference then "OK" else "MISMATCH"
  in
  Printf.printf "%-32s %s\n  generated: %s\n  reference: %s\n" label verdict
    (show_value generated) (show_value reference)

let expressions =
  [
    "1";
    "x";
    "true";
    "1 + 2 + 3";
    "1 + 2 <= 3";
    "a <= b + c";
    "! a && b";
    "! (a + b) <= c && d";
    "1 + (2 + 3)";
  ]

let run spec =
  let grammar = load_grammar spec in
  List.iter
    (fun expr -> check grammar "expr" Reference.parse_expr_exn expr expr)
    expressions

let command =
  Core.Command.basic
    ~summary:"differential test of the grammar-driven parser against impty"
  @@
  let open Core.Command.Let_syntax in
  let open Core.Command.Param in
  let%map spec = flag "-s" (required string) ~doc:"FILE impty base spec file" in
  fun () -> run spec

let () = Command_unix.run command
