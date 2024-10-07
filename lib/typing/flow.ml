type t = Cont | Ret

let merge t1 t2 = match (t1, t2) with Ret, Ret -> Ret | _ -> Cont
