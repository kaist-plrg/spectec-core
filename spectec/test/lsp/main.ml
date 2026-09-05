(** Runs [Check.run] on a few snippets and prints each diagnostic as the LSP
    JSON an editor receives, diffed against [main.expected]. *)

module Lsp = Linol_eio

(* Diagnostics come back grouped by the file they were raised in. Only the
   basename is printed: the group key is an absolute canonical path, which
   differs per machine and per checkout. *)
let print_diagnostics = function
  | [] -> print_endline "(no diagnostics)"
  | groups ->
      List.iter
        (fun (file, ds) ->
          Printf.printf "-- %s\n" (Filename.basename file);
          List.iter
            (fun d ->
              print_endline
                (Yojson.Safe.to_string (Lsp.Diagnostic.yojson_of_t d)))
            ds)
        groups

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
  project_case "cross-file type resolves" "multifile/2-use.spectec";
  (* The message elaboration produces names the constructor, not the culprit;
     the note has to point at the undeclared names inside it. *)
  case "undeclared metavariable"
    "syntax expr =\n\
    \  | ENum int\n\
    \  | EBin expr expr\n\
     relation Eval: |- expr\n\
     rule Eval/bin:\n\
    \  |- EBin e1 e2\n";
  (* A declared metavariable wearing a subscript is fine, and draws no note. *)
  case "subscripted metavariable is fine"
    "syntax expr =\n\
    \  | ENum int\n\
    \  | EBin expr expr\n\
     var e : expr\n\
     relation Eval: |- expr\n\
     rule Eval/bin:\n\
    \  |- EBin e_1 e_2\n";
  (* No marker: the siblings are still elaborated together, ... *)
  project_case "sibling type resolves without a marker" "siblings/2-use.spectec";
  (* ... but only the ones in the same directory, so [bar] stays undefined. *)
  project_case "nested directory stays out" "nested/2-use.spectec"
