(** Generates arbitrary [Il.Value.t] from a spec type, for
    property-based-testing inputs. *)

open Lang.Il

(** {2 Type-based generation} *)

(** [gen_of_typ spec typ] is a generator for arbitrary values of [typ],
    resolving named types through [spec]. Raises on [FuncT] or an unresolved
    named type. *)
val gen_of_typ : spec -> typ -> Value.t Gen.t

(** [gen_of_deftyp spec outer_typ deftyp] is a generator for [deftyp];
    [outer_typ] supplies the type annotation on generated values. *)
val gen_of_deftyp : spec -> typ -> deftyp -> Value.t Gen.t

(** [shrink spec v] returns simpler candidate values of [v] for the shrinker to
    try when minimising a counterexample; [[]] if [v] cannot be shrunk. *)
val shrink : spec -> Value.t -> Value.t list
