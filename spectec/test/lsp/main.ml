(** Runs [Check.run] on a few snippets and prints each diagnostic as the LSP
    JSON an editor receives, diffed against [main.expected]. *)

module Lsp = Linol_eio

let print_diagnostics = function
  | [] -> print_endline "(no diagnostics)"
  | ds ->
      List.iter
        (fun d ->
          print_endline (Yojson.Safe.to_string (Lsp.Diagnostic.yojson_of_t d)))
        ds

let case name text =
  Printf.printf "## %s\n" name;
  print_diagnostics (Spectec_lsp.Check.run ~path:name text);
  print_newline ()

(* The document is one file of a [*.spec]-marked directory; [Check.run] reads
   its siblings from disk, so a type defined in [1-def.spectec] resolves here. *)
let project_case name path =
  Printf.printf "## %s\n" name;
  print_diagnostics
    (Spectec_lsp.Check.run ~path
       (In_channel.with_open_bin path In_channel.input_all));
  print_newline ()

let () =
  case "valid" "syntax type =\n  | INT\n  | type -> type\n";
  case "parse error" "syntax type =\n  | INT\n  | type ->\n";
  case "malformed atom" "syntax t = | X.foo\n";
  case "elab error" "syntax t = foo\n";
  project_case "cross-file type resolves" "multifile/2-use.spectec"
