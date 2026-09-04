module type S = sig
  module Target : Spectec.Target.S

  val command : Core.Command.t
end
