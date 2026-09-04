(** Target_cli.S — the CLI surface of a target: a labeled subcommand ready to
    plug into a parent {!Core.Command.group}.

    The internal structure of [command] is opaque — typically built by composing
    {!Subcommand} constructors, but may be hand-assembled for custom layouts
    (nested subcommand groups, custom hooks). *)

module type S = sig
  module Target : Spectec.Target.S

  val command : Core.Command.t
end
