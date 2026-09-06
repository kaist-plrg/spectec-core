(** Verify preview text, source navigation, and stale renders. *)

let read path = In_channel.with_open_bin path In_channel.input_all

(* Paths are absolute and machine-specific once [Check.canonical] has resolved
   them, so print them relative to the fixture. *)
let shorten path =
  match String.rindex_opt path '/' with
  | Some i -> String.sub path (i + 1) (String.length path - i - 1)
  | None -> path

(* A render names its sources by absolute path, which is this machine's rather
   than the fixture's. Both fixture directories sit in this one, so trimming it
   off covers them. *)
let scrub =
  let dir =
    Filename.dirname
      (Filename.dirname (Spectec_lsp.Check.canonical "multifile/2-use.spectec"))
    ^ "/"
  in
  fun text ->
    let width = String.length dir in
    let buffer = Buffer.create (String.length text) in
    let i = ref 0 in
    while !i < String.length text do
      if
        !i + width <= String.length text
        && String.equal (String.sub text !i width) dir
      then (
        Buffer.add_string buffer "<test>/";
        i := !i + width)
      else (
        Buffer.add_char buffer text.[!i];
        incr i)
    done;
    Buffer.contents buffer

let kind_of_depth = function 0 -> "def " | 1 -> "rule" | _ -> "step"

let print_render ?(stage = "IL") label (render : Spectec_lsp.Preview.render) =
  Printf.printf "## %s\n" label;
  Printf.printf "stale: %b\n" render.stale;
  (match render.reason with
  | None -> ()
  | Some reason ->
      Printf.printf "reason: %s (%s:%d)\n" reason.message
        (shorten reason.region.left.file)
        reason.region.left.line);
  (match render.text with
  | "" -> print_endline "(nothing rendered yet)"
  | text ->
      Printf.printf "--- %s ---\n" stage;
      print_string (scrub text);
      print_newline ());
  print_endline "--- definitions ---";
  (match render.entries with
  | [] -> print_endline "  (none)"
  | entries ->
      List.iter
        (fun (entry : Spectec_lsp.Preview.entry) ->
          Printf.printf "  %s line %-3d %s <- %s:%d:%d\n" stage entry.line
            (kind_of_depth entry.depth)
            (shorten entry.region.left.file)
            entry.region.left.line entry.region.left.column)
        entries);
  print_newline ()

let () =
  let cache = Spectec_lsp.Preview.create () in
  let path = "multifile/2-use.spectec" in
  let text = read path in
  (* The spec is the marked directory, not the open file: [foo] is declared in
     the sibling, and both have to appear in the render for this to be the same
     thing [spectecx elab] would print. *)
  print_render "whole spec, from one of its files"
    (Spectec_lsp.Preview.render cache ~open_path:path ~text);
  (* The buffer stops elaborating, as it does for most of the time it is being
     edited. The pane must not blank. *)
  print_render "buffer no longer elaborates"
    (Spectec_lsp.Preview.render cache ~open_path:path
       ~text:"syntax t = undeclared\n");
  (* And comes back when it does. *)
  print_render "back to a spec that elaborates"
    (Spectec_lsp.Preview.render cache ~open_path:path ~text);
  (* A file that has never elaborated has nothing to fall back on, and says so
     rather than showing an empty pane with no explanation. *)
  let fresh = Spectec_lsp.Preview.create () in
  print_render "never elaborated"
    (Spectec_lsp.Preview.render fresh ~open_path:path
       ~text:"syntax t = undeclared\n");
  (* Distinct rule branches retain their own source mappings. *)
  let cache = Spectec_lsp.Preview.create () in
  let path = "relation/eval.spectec" in
  let text = read path in
  List.iter
    (fun (stage, name) ->
      print_render ~stage:name
        (Printf.sprintf "a relation of three rules, as %s" name)
        (Spectec_lsp.Preview.render ~stage cache ~open_path:path ~text))
    [
      (Spectec_lsp.Preview.Il, "IL");
      (Spectec_lsp.Preview.Sl, "SL");
      (Spectec_lsp.Preview.Pl, "PL");
    ]
