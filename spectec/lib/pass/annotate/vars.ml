open Common.Source
open Lang.Il

(* Variables compare by name and iteration dimensions only. The type is carried
   for reconstruction but ignored for identity. *)
module Var = struct
  type t = var

  let compare (var_a : t) (var_b : t) =
    match String.compare var_a.varid.it var_b.varid.it with
    | 0 -> compare var_a.iters var_b.iters
    | n -> n

  let equal (var_a : t) (var_b : t) : bool = compare var_a var_b = 0
end

module VarSet = Set.Make (Var)

let empty = VarSet.empty
let ( + ) = VarSet.union
let unions (sets : VarSet.t list) : VarSet.t = List.fold_left ( + ) empty sets

let rec free_exp (exp : exp) : VarSet.t =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> empty
  | VarE id ->
      VarSet.singleton { varid = id; typ = exp.note $ exp.at; iters = [] }
  | UnE (_, _, exp) -> free_exp exp
  | BinE (_, _, exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | CmpE (_, _, exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | UpCastE (_, exp) -> free_exp exp
  | DownCastE (_, exp) -> free_exp exp
  | SubE (exp, _) -> free_exp exp
  | MatchE (exp, _) -> free_exp exp
  | TupleE exps -> free_exps exps
  | CaseE notexp -> free_exps (Mixfix.args notexp)
  | StrE expfields -> expfields |> List.map snd |> free_exps
  | OptE (Some exp) -> free_exp exp
  | OptE None -> empty
  | ListE exps -> free_exps exps
  | ConsE (exp_h, exp_t) -> free_exp exp_h + free_exp exp_t
  | CatE (exp_l, exp_r) -> free_exp exp_l + free_exp exp_r
  | MemE (exp_e, exp_s) -> free_exp exp_e + free_exp exp_s
  | LenE exp -> free_exp exp
  | DotE (exp, _) -> free_exp exp
  | IdxE (exp_b, exp_i) -> free_exp exp_b + free_exp exp_i
  | SliceE (exp_b, exp_l, exp_h) ->
      free_exp exp_b + free_exp exp_l + free_exp exp_h
  | UpdE (exp_b, path, exp_f) ->
      free_exp exp_b + free_path path + free_exp exp_f
  | CallE (_, _, args) -> free_args args
  | IterE (exp, (iter, itervars)) ->
      free_exp exp
      |> VarSet.map (fun var ->
             if List.exists (Var.equal var) itervars then
               { var with iters = var.iters @ [ iter ] }
             else var)

and free_exps (exps : exp list) : VarSet.t = unions (List.map free_exp exps)

and free_path (path : path) : VarSet.t =
  match path.it with
  | RootP -> empty
  | IdxP (path, exp) -> free_path path + free_exp exp
  | SliceP (path, exp_l, exp_h) ->
      free_path path + free_exp exp_l + free_exp exp_h
  | DotP (path, _) -> free_path path

and free_arg (arg : arg) : VarSet.t =
  match arg.it with ExpA exp -> free_exp exp | DefA _ -> empty

and free_args (args : arg list) : VarSet.t = unions (List.map free_arg args)
