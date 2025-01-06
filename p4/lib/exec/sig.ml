module Value = Runtime_static.Value

type t = Cont | Ret of Value.t option | Exit
