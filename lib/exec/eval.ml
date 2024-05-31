open Runtime.Base
open Runtime.Context

let rec eval_simplify_type (ctx : Ctx.t) (typ : Type.t) : Type.t =
  match typ with
  | NameT name -> Ctx.find_td name ctx |> Option.get |> eval_simplify_type ctx
  | NewT name -> Ctx.find_td name ctx |> Option.get
  | _ -> typ
