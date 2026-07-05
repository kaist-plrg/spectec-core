(** Where a target's spec files come from: an explicit list of files, or a
    directory to collect. *)

type t = Files of string list | Dir of string

(** [files src] is the spec files for [src]: a {!Files} list as given, or the
    [.spectec] files under a {!Dir} via {!Spectec.collect_spec_files}. Errors
    when the directory is missing or is not a directory. *)
val files : t -> (string list, Spectec.Error.t) result
