(** Diagnostic rendering — single layout, optional ANSI color.

    Separate from {!Record} because rendering touches file I/O (for source
    snippets) and ANSI styling, neither of which belongs in the structured
    diagnostic representation. The same layout is produced whether color is on
    or off; only the styling differs. *)

(** Render a single diagnostic. The [cache] is used to look up source snippets
    for the location section. *)
val render : ansi:Ansi.t -> cache:Source_cache.t -> Record.t -> string

(** Render a bag of diagnostics, one per entry, in sorted order. Manages an
    internal {!Source_cache.t} so callers don't have to. *)
val render_bag : ansi:Ansi.t -> Record.Bag.t -> string

(** Render each diagnostic as severity, headline, and nested trace messages only
    -- no locations, code, notes, or related entries. For interpreter failure
    traces in the test corpus: keeps the failure reasons and their nesting while
    staying stable across spec line shifts. *)
val render_bag_trace : Record.Bag.t -> string
