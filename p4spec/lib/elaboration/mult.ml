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

let union (occurs_a : t) (occurs_b : t) : t =
  VEnv.union
    (fun _ iters_a iters_b ->
      if List.length iters_a < List.length iters_b then Some iters_a
      else Some iters_b)
    occurs_a occurs_b

let collect_itervars (bounds : t) (occurs : t) (iter : iter) :
    (Id.t * Dom.Dim.t) list =
  occurs |> VEnv.bindings
  |> List.filter_map (fun var ->
         let id, iters = var in
         let iters = iters @ [ iter ] in
         let iters_expect = VEnv.find id bounds in
         if Dom.Dim.sub iters iters_expect then Some var else None)

(* Annotate iterated expressions with their bound identifiers,
   where iterations bind from inside to outside,
   and it is an error to have superfluous or missing iterations *)

let bind (binder : t -> 'a -> ('a * t) attempt) (bounds : t) (construct : 'a) :
    'a attempt =
  let* construct, occurs = binder bounds construct in
  VEnv.fold
    (fun id iters construct ->
      let* construct = construct in
      let iters_expect = VEnv.find id bounds in
      if not (Dom.Dim.equiv iters iters_expect) then
        fail id.at
          ("mismatched iteration dimensions for identifier " ^ Id.to_string id
         ^ ": expected "
          ^ Dom.Dim.to_string iters_expect
          ^ ", got " ^ Dom.Dim.to_string iters)
      else Ok construct)
    occurs (Ok construct)

(* Expression *)

let rec bind_exp (bounds : t) (exp : exp) : (exp * t) attempt =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE _ | NumE _ | TextE _ -> Ok (exp, empty)
  | VarE id ->
      if VEnv.mem id bounds then Ok (exp, singleton id)
      else fail exp.at ("free identifier: " ^ Id.to_string id)
  | UnE (op, optyp, exp) ->
      let* exp, occurs = bind_exp bounds exp in
      let exp = UnE (op, optyp, exp) $$ (at, note) in
      Ok (exp, occurs)
  | BinE (op, bintyp, exp_l, exp_r) ->
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_r, occurs_r = bind_exp bounds exp_r in
      let exp = BinE (op, bintyp, exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      Ok (exp, occurs)
  | CmpE (op, cmptyp, exp_l, exp_r) ->
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_r, occurs_r = bind_exp bounds exp_r in
      let exp = CmpE (op, cmptyp, exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      Ok (exp, occurs)
  | TupleE exps ->
      let* exps, occurs = bind_exps bounds exps in
      let exp = TupleE exps $$ (at, note) in
      Ok (exp, occurs)
  | CaseE notexp ->
      let mixop, exps = notexp in
      let* exps, occurs = bind_exps bounds exps in
      let notexp = (mixop, exps) in
      let exp = CaseE notexp $$ (at, note) in
      Ok (exp, occurs)
  | OptE exp_opt -> (
      match exp_opt with
      | Some exp ->
          let* exp, occurs = bind_exp bounds exp in
          let exp_opt = Some exp in
          let exp = OptE exp_opt $$ (at, note) in
          Ok (exp, occurs)
      | None -> Ok (exp, empty))
  | StrE expfields ->
      let atoms, exps = List.split expfields in
      let* exps, occurs = bind_exps bounds exps in
      let expfields = List.combine atoms exps in
      let exp = StrE expfields $$ (at, note) in
      Ok (exp, occurs)
  | DotE (exp, atom) ->
      let* exp, occurs = bind_exp bounds exp in
      let exp = DotE (exp, atom) $$ (at, note) in
      Ok (exp, occurs)
  | ListE exps ->
      let* exps, occurs = bind_exps bounds exps in
      let exp = ListE exps $$ (at, note) in
      Ok (exp, occurs)
  | ConsE (exp_l, exp_r) ->
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_r, occurs_r = bind_exp bounds exp_r in
      let exp = ConsE (exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      Ok (exp, occurs)
  | CatE (exp_l, exp_r) ->
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_r, occurs_r = bind_exp bounds exp_r in
      let exp = CatE (exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      Ok (exp, occurs)
  | MemE (exp_l, exp_r) ->
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_r, occurs_r = bind_exp bounds exp_r in
      let exp = MemE (exp_l, exp_r) $$ (at, note) in
      let occurs = union occurs_l occurs_r in
      Ok (exp, occurs)
  | LenE exp ->
      let* exp, occurs = bind_exp bounds exp in
      let exp = LenE exp $$ (at, note) in
      Ok (exp, occurs)
  | IdxE (exp_b, exp_i) ->
      let* exp_b, occurs_b = bind_exp bounds exp_b in
      let* exp_i, occurs_i = bind_exp bounds exp_i in
      let exp = IdxE (exp_b, exp_i) $$ (at, note) in
      let occurs = union occurs_b occurs_i in
      Ok (exp, occurs)
  | SliceE (exp_b, exp_l, exp_h) ->
      let* exp_b, occurs_b = bind_exp bounds exp_b in
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_h, occurs_h = bind_exp bounds exp_h in
      let exp = SliceE (exp_b, exp_l, exp_h) $$ (at, note) in
      let occurs = union (union occurs_b occurs_l) occurs_h in
      Ok (exp, occurs)
  | UpdE (exp_b, path, exp_f) ->
      let* exp_b, occurs_b = bind_exp bounds exp_b in
      let* exp_f, occurs_f = bind_exp bounds exp_f in
      let* path, occurs_p = bind_path bounds path in
      let exp = UpdE (exp_b, path, exp_f) $$ (at, note) in
      let occurs = union (union occurs_b occurs_f) occurs_p in
      Ok (exp, occurs)
  | CallE (id, targs, args) ->
      let* args, occurs = bind_args bounds args in
      let exp = CallE (id, targs, args) $$ (at, note) in
      Ok (exp, occurs)
  | IterE (_, (_, (_ :: _ as binds))) ->
      fail at
        ("iterated expression should initially have no annotations, but got "
        ^ String.concat ", " (List.map Il.Print.string_of_var binds))
  | IterE (exp, (iter, [])) -> (
      let* exp, occurs = bind_exp bounds exp in
      let itervars = collect_itervars bounds occurs iter in
      match itervars with
      | [] -> fail at "empty iteration"
      | _ ->
          let exp = IterE (exp, (iter, itervars)) $$ (at, note) in
          let occurs =
            List.fold_left
              (fun occurs (id, iters) -> VEnv.add id (iters @ [ iter ]) occurs)
              occurs itervars
          in
          Ok (exp, occurs))
  | CastE (exp, typ) ->
      let* exp, occurs = bind_exp bounds exp in
      let exp = CastE (exp, typ) $$ (at, note) in
      Ok (exp, occurs)

and bind_exps (bounds : t) (exps : exp list) : (exp list * t) attempt =
  match exps with
  | [] -> Ok ([], empty)
  | exp :: exps ->
      let* exp, occurs_h = bind_exp bounds exp in
      let* exps, occurs_t = bind_exps bounds exps in
      let exps = exp :: exps in
      let occurs = union occurs_h occurs_t in
      Ok (exps, occurs)

(* Path *)

and bind_path (bounds : t) (path : path) : (path * t) attempt =
  let at, note = (path.at, path.note) in
  match path.it with
  | RootP -> Ok (path, empty)
  | IdxP (path, exp) ->
      let* path, occurs_p = bind_path bounds path in
      let* exp, occurs_e = bind_exp bounds exp in
      let path = IdxP (path, exp) $$ (at, note) in
      let occurs = union occurs_p occurs_e in
      Ok (path, occurs)
  | SliceP (path, exp_l, exp_h) ->
      let* path, occurs_p = bind_path bounds path in
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_h, occurs_h = bind_exp bounds exp_h in
      let path = SliceP (path, exp_l, exp_h) $$ (at, note) in
      let occurs = union (union occurs_p occurs_l) occurs_h in
      Ok (path, occurs)
  | DotP (path, atom) ->
      let* path, occurs = bind_path bounds path in
      let path = DotP (path, atom) $$ (at, note) in
      Ok (path, occurs)

(* Argument *)

and bind_arg (bounds : t) (arg : arg) : (arg * t) attempt =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let* exp, occurs = bind_exp bounds exp in
      let arg = ExpA exp $ at in
      Ok (arg, occurs)
  | DefA _ -> Ok (arg, empty)

and bind_args (bounds : t) (args : arg list) : (arg list * t) attempt =
  match args with
  | [] -> Ok ([], empty)
  | arg :: args ->
      let* arg, occurs_h = bind_arg bounds arg in
      let* args, occurs_t = bind_args bounds args in
      let args = arg :: args in
      let occurs = union occurs_h occurs_t in
      Ok (args, occurs)

(* Premise *)

and bind_prem (binds : t) (bounds : t) (prem : prem) : (prem * t) attempt =
  let at = prem.at in
  match prem.it with
  | RulePr (id, notexp) ->
      let mixop, exps = notexp in
      let* exps, occurs = bind_exps bounds exps in
      let notexp = (mixop, exps) in
      let prem = RulePr (id, notexp) $ at in
      Ok (prem, occurs)
  | IfPr exp ->
      let* exp, occurs = bind_exp bounds exp in
      let prem = IfPr exp $ at in
      Ok (prem, occurs)
  | ElsePr -> Ok (prem, empty)
  | LetPr (exp_l, exp_r) ->
      let* exp_l, occurs_l = bind_exp bounds exp_l in
      let* exp_r, occurs_r = bind_exp bounds exp_r in
      let prem = LetPr (exp_l, exp_r) $ at in
      let occurs = union occurs_l occurs_r in
      Ok (prem, occurs)
  | IterPr (_, (_, _ :: _)) ->
      fail at "iterated premise should initially have no annotations"
  | IterPr (prem, (iter, [])) -> (
      let* prem, occurs = bind_prem binds bounds prem in
      let itervars = collect_itervars bounds occurs iter in
      match itervars with
      | [] -> fail at "empty iteration"
      | _
        when List.for_all
               (fun (id, iters) ->
                 match VEnv.find_opt id binds with
                 | Some iters_bind -> Dom.Dim.sub iters iters_bind
                 | None -> false)
               itervars ->
          fail at
            ("cannot determine dimension of binding identifier(s) only: "
            ^ String.concat ", " (List.map Il.Print.string_of_var itervars))
      | _ ->
          let prem = IterPr (prem, (iter, itervars)) $ at in
          let occurs =
            List.fold_left
              (fun occurs (id, iters) -> VEnv.add id (iters @ [ iter ]) occurs)
              occurs itervars
          in
          Ok (prem, occurs))

and bind_prems (binds : t) (bounds : t) (prems : prem list) :
    (prem list * t) attempt =
  match prems with
  | [] -> Ok ([], empty)
  | prem :: prems ->
      let* prem, occurs_h = bind_prem binds bounds prem in
      let* prems, occurs_t = bind_prems binds bounds prems in
      let prems = prem :: prems in
      let occurs = union occurs_h occurs_t in
      Ok (prems, occurs)
