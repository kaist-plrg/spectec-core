open Domain.Dom
open Il.Ast
open Attempt
open Envs
open Util.Source

type t = VEnv.t

(* Constructors *)

let empty = VEnv.empty
let singleton id = VEnv.add id [] VEnv.empty
let ( + ) = VEnv.union

let union_frees (at : region) (free_a : t) (free_b : t) : t attempt =
  let ids_a = VEnv.bindings free_a |> List.map fst |> IdSet.of_list in
  let ids_b = VEnv.bindings free_b |> List.map fst |> IdSet.of_list in
  let ids = IdSet.inter ids_a ids_b in
  if IdSet.is_empty ids then
    Ok (VEnv.union (fun _ -> assert false) free_a free_b)
  else
    fail at
      ("multiple bindings for the same identifier(s): "
      ^ (ids |> IdSet.elements |> List.map Id.to_string |> String.concat ", "))

let union_occurs (occurs_a : t) (occurs_b : t) : t =
  VEnv.union
    (fun _ iters_a iters_b ->
      if List.length iters_a < List.length iters_b then Some iters_a
      else Some iters_b)
    occurs_a occurs_b

(* Collect binding identifiers,
   while enforcing the invariant that binding identifiers can only occur in
   trivially invertible constructs, or binder patterns *)

let free_nontrivial (at : region) (construct : string) (frees : t) : t attempt =
  if VEnv.is_empty frees then Ok empty
  else fail at ("invalid binding position for non-invertible " ^ construct)

(* Expressions *)

let rec free_exp (bounds : t) (exp : exp) : t attempt =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> Ok empty
  | VarE id -> if VEnv.mem id bounds then Ok empty else Ok (singleton id)
  | UnE (_, _, exp) ->
      let* frees = free_exp bounds exp in
      free_nontrivial exp.at "unary operator" frees
  | BinE (_, _, exp_l, exp_r) ->
      let* free_l = free_exp bounds exp_l in
      let* free_r = free_exp bounds exp_r in
      let* frees = union_frees exp.at free_l free_r in
      free_nontrivial exp.at "binary operator" frees
  | CmpE (_, _, exp_l, exp_r) ->
      let* free_l = free_exp bounds exp_l in
      let* free_r = free_exp bounds exp_r in
      let* frees = union_frees exp.at free_l free_r in
      free_nontrivial exp.at "comparison operator" frees
  | TupleE exps -> free_exps bounds exps
  | CaseE notexp -> notexp |> snd |> free_exps bounds
  | OptE exp_opt ->
      exp_opt
      |> Option.map (free_exp bounds)
      |> Option.value ~default:(Ok empty)
  | StrE expfields -> expfields |> List.map snd |> free_exps bounds
  | DotE (exp, _) ->
      let* frees = free_exp bounds exp in
      free_nontrivial exp.at "dot operator" frees
  | ListE exps -> free_exps bounds exps
  | ConsE (exp_l, exp_r) ->
      let* free_l = free_exp bounds exp_l in
      let* free_r = free_exp bounds exp_r in
      union_frees exp.at free_l free_r
  | CatE (exp_l, exp_r) ->
      let* free_l = free_exp bounds exp_l in
      let* free_r = free_exp bounds exp_r in
      let* frees = union_frees exp.at free_l free_r in
      free_nontrivial exp.at "concatenation operator" frees
  | MemE (exp_l, exp_r) ->
      let* free_l = free_exp bounds exp_l in
      let* free_r = free_exp bounds exp_r in
      let* frees = union_frees exp.at free_l free_r in
      free_nontrivial exp.at "set membership operator" frees
  | LenE exp ->
      let* frees = free_exp bounds exp in
      free_nontrivial exp.at "length operator" frees
  | IdxE (exp_b, exp_i) ->
      let* free_b = free_exp bounds exp_b in
      let* free_i = free_exp bounds exp_i in
      let* frees = union_frees exp.at free_b free_i in
      free_nontrivial exp.at "indexing operator" frees
  | SliceE (exp_b, exp_l, exp_h) ->
      let* free_b = free_exp bounds exp_b in
      let* free_l = free_exp bounds exp_l in
      let* free_h = free_exp bounds exp_h in
      let* frees = union_frees exp.at free_b free_l in
      let* frees = union_frees exp.at frees free_h in
      free_nontrivial exp.at "slicing operator" frees
  | UpdE (exp_b, path, exp_f) ->
      let* free_b = free_exp bounds exp_b in
      let* free_f = free_exp bounds exp_f in
      let* free_p = free_path bounds path in
      let* frees = union_frees exp.at free_b free_f in
      let* frees = union_frees exp.at frees free_p in
      free_nontrivial exp.at "update operator" frees
  | CallE (_, _, args) ->
      let* frees = free_args bounds args in
      free_nontrivial exp.at "call operator" frees
  | IterE (exp, (iter, _)) ->
      let* frees = free_exp bounds exp in
      let frees = VEnv.map (fun iters -> iters @ [ iter ]) frees in
      Ok frees
  | CastE (exp, _) -> free_exp bounds exp

and free_exps (bounds : t) (exps : exp list) : t attempt =
  match exps with
  | [] -> Ok empty
  | exp :: exps ->
      let* free_h = free_exp bounds exp in
      let* free_t = free_exps bounds exps in
      union_frees exp.at free_h free_t

(* Path *)

and free_path (bounds : t) (path : path) : t attempt =
  match path.it with
  | RootP -> Ok empty
  | IdxP (path, exp) ->
      let* free_p = free_path bounds path in
      let* free_e = free_exp bounds exp in
      let* frees = union_frees path.at free_p free_e in
      free_nontrivial path.at "indexing operator" frees
  | SliceP (path, exp_l, exp_h) ->
      let* free_p = free_path bounds path in
      let* free_l = free_exp bounds exp_l in
      let* free_h = free_exp bounds exp_h in
      let* frees = union_frees path.at free_p free_l in
      let* frees = union_frees path.at frees free_h in
      free_nontrivial path.at "slice operator" frees
  | DotP (path, _) ->
      let* frees = free_path bounds path in
      free_nontrivial path.at "dot operator" frees

(* Arguments *)

and free_arg (bounds : t) (arg : arg) : t attempt =
  match arg.it with ExpA exp -> free_exp bounds exp | DefA _ -> Ok empty

and free_args (bounds : t) (args : arg list) : t attempt =
  match args with
  | [] -> Ok empty
  | arg :: args ->
      let* free_h = free_arg bounds arg in
      let* free_t = free_args bounds args in
      union_frees arg.at free_h free_t

(* If premises *)

type kind = [ `Equality | `AssignL of t | `AssignR of t ]

let free_if_prem (bounds : t) (at : region) (exp_l : exp) (exp_r : exp) :
    kind attempt =
  let* free_l = free_exp bounds exp_l in
  let* free_r = free_exp bounds exp_r in
  if VEnv.is_empty free_l && VEnv.is_empty free_r then Ok `Equality
  else if VEnv.is_empty free_r then Ok (`AssignL free_l)
  else if VEnv.is_empty free_l then Ok (`AssignR free_r)
  else fail at "cannot bind both sides of an equality"

(* Annotate iterated expressions with their bound identifiers *)

let bind (binder : t -> 'a -> ('a * t) attempt) (bounds : t) (construct : 'a) :
    'a attempt =
  let* construct, occurs = binder bounds construct in
  VEnv.iter
    (fun id iters ->
      let iters_expect = VEnv.find id bounds in
      if not (Dom.Dim.equiv iters iters_expect) then
        print_endline
          ("mismatched iteration dimensions for identifier " ^ Id.to_string id
         ^ ": expected "
          ^ Dom.Dim.to_string iters_expect
          ^ ", got " ^ Dom.Dim.to_string iters)
      else ())
    occurs;
  Ok construct

(* Expression *)

let rec bind_exp (bounds : t) (exp : exp) : (exp * t) attempt =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> Ok (exp, empty)
  | VarE id ->
      if VEnv.mem id bounds then Ok (exp, singleton id)
      else fail exp.at ("free identifier: " ^ Id.to_string id)
  | UnE (op, optyp, exp) ->
      let* exp, occurs = bind_exp bounds exp in
      let exp = { exp with it = UnE (op, optyp, exp) } in
      Ok (exp, occurs)
  | BinE (op, bintyp, exp_l, exp_r) ->
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_r, occurs_r = bind_exp bounds exp_r in
      let exp = { exp with it = BinE (op, bintyp, exp_l, exp_r) } in
      let occurs = union_occurs occurs_l occurs_r in
      Ok (exp, occurs)
  | CmpE (op, cmptyp, exp_l, exp_r) ->
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_r, occurs_r = bind_exp bounds exp_r in
      let exp = { exp with it = CmpE (op, cmptyp, exp_l, exp_r) } in
      let occurs = union_occurs occurs_l occurs_r in
      Ok (exp, occurs)
  | TupleE exps ->
      let* exps, occurs = bind_exps bounds exps in
      let exp = { exp with it = TupleE exps } in
      Ok (exp, occurs)
  | CaseE notexp ->
      let mixop, exps = notexp in
      let* exps, occurs = bind_exps bounds exps in
      let notexp = (mixop, exps) in
      let exp = { exp with it = CaseE notexp } in
      Ok (exp, occurs)
  | OptE exp_opt -> (
      match exp_opt with
      | Some exp ->
          let* exp, occurs = bind_exp bounds exp in
          let exp_opt = Some exp in
          let exp = { exp with it = OptE exp_opt } in
          Ok (exp, occurs)
      | None -> Ok (exp, empty))
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let* exps, occurs = bind_exps bounds exps in
      let expfields = List.combine atoms exps in
      let exp = { exp with it = StrE expfields } in
      Ok (exp, occurs)
  | DotE (exp, atom) ->
      let* exp, occurs = bind_exp bounds exp in
      let exp = { exp with it = DotE (exp, atom) } in
      Ok (exp, occurs)
  | ListE exps ->
      let* exps, occurs = bind_exps bounds exps in
      let exp = { exp with it = ListE exps } in
      Ok (exp, occurs)
  | ConsE (exp_l, exp_r) ->
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_r, occurs_r = bind_exp bounds exp_r in
      let exp = { exp with it = ConsE (exp_l, exp_r) } in
      let occurs = union_occurs occurs_l occurs_r in
      Ok (exp, occurs)
  | CatE (exp_l, exp_r) ->
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_r, occurs_r = bind_exp bounds exp_r in
      let exp = { exp with it = CatE (exp_l, exp_r) } in
      let occurs = union_occurs occurs_l occurs_r in
      Ok (exp, occurs)
  | MemE (exp_l, exp_r) ->
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_r, occurs_r = bind_exp bounds exp_r in
      let exp = { exp with it = MemE (exp_l, exp_r) } in
      let occurs = union_occurs occurs_l occurs_r in
      Ok (exp, occurs)
  | LenE exp ->
      let* exp, occurs = bind_exp bounds exp in
      let exp = { exp with it = LenE exp } in
      Ok (exp, occurs)
  | IdxE (exp_b, exp_i) ->
      let* exp_b, occurs_b = bind_exp bounds exp_b in
      let* exp_i, occurs_i = bind_exp bounds exp_i in
      let exp = { exp with it = IdxE (exp_b, exp_i) } in
      let occurs = union_occurs occurs_b occurs_i in
      Ok (exp, occurs)
  | SliceE (exp_b, exp_l, exp_h) ->
      let* exp_b, occurs_b = bind_exp bounds exp_b in
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_h, occurs_h = bind_exp bounds exp_h in
      let exp = { exp with it = SliceE (exp_b, exp_l, exp_h) } in
      let occurs = union_occurs (union_occurs occurs_b occurs_l) occurs_h in
      Ok (exp, occurs)
  | UpdE (exp_b, path, exp_f) ->
      let* exp_b, occurs_b = bind_exp bounds exp_b in
      let* exp_f, occurs_f = bind_exp bounds exp_f in
      let* path, occurs_p = bind_path bounds path in
      let exp = { exp with it = UpdE (exp_b, path, exp_f) } in
      let occurs = union_occurs (union_occurs occurs_b occurs_f) occurs_p in
      Ok (exp, occurs)
  | CallE (id, targs, args) ->
      let* args, occurs = bind_args bounds args in
      let exp = { exp with it = CallE (id, targs, args) } in
      Ok (exp, occurs)
  | IterE (_, (_, _ :: _)) ->
      fail exp.at "iterated expression should initially have no annotations"
  | IterE (exp, (iter, [])) -> (
      let* exp, occurs = bind_exp bounds exp in
      let binds =
        occurs |> VEnv.bindings
        |> List.filter_map (fun var ->
               let id, iters = var in
               let iters = iters @ [ iter ] in
               let iters_expect = VEnv.find id bounds in
               if Dom.Dim.sub iters iters_expect then Some var else None)
      in
      match binds with
      | [] -> fail exp.at "empty iteration"
      | _ ->
          let exp = { exp with it = IterE (exp, (iter, binds)) } in
          let occurs =
            List.fold_left
              (fun occurs (id, iters) -> VEnv.add id (iters @ [ iter ]) occurs)
              occurs binds
          in
          Ok (exp, occurs))
  | CastE (exp, typ) ->
      let* exp, occurs = bind_exp bounds exp in
      let exp = { exp with it = CastE (exp, typ) } in
      Ok (exp, occurs)

and bind_exps (bounds : t) (exps : exp list) : (exp list * t) attempt =
  match exps with
  | [] -> Ok ([], empty)
  | exp :: exps ->
      let* exp, occurs_h = bind_exp bounds exp in
      let* exps, occurs_t = bind_exps bounds exps in
      let exps = exp :: exps in
      let occurs = union_occurs occurs_h occurs_t in
      Ok (exps, occurs)

(* Path *)

and bind_path (bounds : t) (path : path) : (path * t) attempt =
  match path.it with
  | RootP -> Ok (path, empty)
  | IdxP (path, exp) ->
      let* path, occurs_p = bind_path bounds path in
      let* exp, occurs_e = bind_exp bounds exp in
      let path = { path with it = IdxP (path, exp) } in
      let occurs = union_occurs occurs_p occurs_e in
      Ok (path, occurs)
  | SliceP (path, exp_l, exp_h) ->
      let* path, occurs_p = bind_path bounds path in
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_h, occurs_h = bind_exp bounds exp_h in
      let path = { path with it = SliceP (path, exp_l, exp_h) } in
      let occurs = union_occurs (union_occurs occurs_p occurs_l) occurs_h in
      Ok (path, occurs)
  | DotP (path, atom) ->
      let* path, occurs = bind_path bounds path in
      let path = { path with it = DotP (path, atom) } in
      Ok (path, occurs)

(* Arguments *)

and bind_arg (bounds : t) (arg : arg) : (arg * t) attempt =
  match arg.it with
  | ExpA exp ->
      let* exp, occurs = bind_exp bounds exp in
      let arg = { arg with it = ExpA exp } in
      Ok (arg, occurs)
  | DefA _ -> Ok (arg, empty)

and bind_args (bounds : t) (args : arg list) : (arg list * t) attempt =
  match args with
  | [] -> Ok ([], empty)
  | arg :: args ->
      let* arg, occurs_h = bind_arg bounds arg in
      let* args, occurs_t = bind_args bounds args in
      let args = arg :: args in
      let occurs = union_occurs occurs_h occurs_t in
      Ok (args, occurs)
