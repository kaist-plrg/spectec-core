(** A printer for object-language concrete syntax, the inverse of {!Parse}.

    Given a [terminal_printer] that renders each grammar terminal as surface
    text, [run] turns an IL value back into a string that re-parses to the same
    value, inserting the fewest grouping parentheses the precedence chain
    allows. *)

(** Render one grammar terminal as surface text: the inverse of a
    {!Parse.token_classifier}. *)
type terminal_printer = Grammar.Terminal.t -> string

exception Error of string

(** [run print_terminal grammar start value] renders [value] as an instance of
    syntax [start]. Raises {!Error} when [value] matches no production of the
    syntax it reaches, and [Failure] when [value] is not a case value or the
    grammar references a syntax that does not exist. *)
val run : terminal_printer -> Grammar.t -> string -> Il.value -> string
