open Lang.Il

(** Manually-implemented generators for impty's [builtin generator]
    declarations, keyed by EL identifier name.

    Registered names:
    - ["base_prog"] - well-typed Impty program (INT and BOOL only)
    - ["functions_prog"] - well-typed Impty program with first-class functions
    - ["curried_prog"] - program exercising curried closure calls, targeting the
      [env_clo] evaluation rule *)
val manual_gens :
  (string * (spec -> (string * value) list Quickcheck.Gen.t)) list
