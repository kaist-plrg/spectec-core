(** Runs [Check.run] on a few snippets and prints each diagnostic as the LSP
    JSON an editor receives, diffed against [main.expected]. *)

module Lsp = Linol_eio

let case name text =
  Printf.printf "## %s\n" name;
  (match Spectec_lsp.Check.run ~origin:name text with
  | [] -> print_endline "(no diagnostics)"
  | ds ->
      List.iter
        (fun d ->
          print_endline (Yojson.Safe.to_string (Lsp.Diagnostic.yojson_of_t d)))
        ds);
  print_newline ()

let () =
  case "valid" "syntax type =\n  | INT\n  | type -> type\n";
  case "parse error" "syntax type =\n  | INT\n  | type ->\n";
  case "elab error" "syntax t = foo\n"
