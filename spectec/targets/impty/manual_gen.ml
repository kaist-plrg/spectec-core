open Lang.Il
open Common.Source
open Value
open Quickcheck

(* ===== Shared value construction helpers ===== *)

let id_val name = case_v ~var:"id" [ atom "`ID"; arg (text name) ]

(* expr values: literal and id cases are FLAT in the IL (elab_typcase_plain
   expands | literal and | id into expr's VariantT directly, no wrapper) *)
let expr_num n = case_v ~var:"expr" [ atom "`NUM"; arg (int (Bigint.of_int n)) ]
let expr_bool b = case_v ~var:"expr" [ atom "`BOOL"; arg (bool b) ]
let expr_var s = case_v ~var:"expr" [ atom "`ID"; arg (text s) ]
let expr_add e1 e2 = case_v ~var:"expr" [ arg e1; atom "+"; arg e2 ]
let expr_leq e1 e2 = case_v ~var:"expr" [ arg e1; atom "<="; arg e2 ]
let expr_not e = case_v ~var:"expr" [ atom "!"; arg e ]
let expr_and e1 e2 = case_v ~var:"expr" [ arg e1; atom "&&"; arg e2 ]

(* closure-only expr constructors *)
let expr_fun ta xa tr e =
  case_v ~var:"expr"
    [
      atom "FUN";
      atom "(";
      arg ta;
      arg xa;
      atom ")";
      atom "`->";
      arg tr;
      atom "{";
      arg e;
      atom "}";
    ]

let expr_call ef ea = case_v ~var:"expr" [ arg ef; atom "("; arg ea; atom ")" ]
let cmd_skip = case_v ~var:"command" [ atom "SKIP" ]
let cmd_decl ty id e = case_v ~var:"command" [ arg ty; arg id; atom "="; arg e ]
let cmd_assign id e = case_v ~var:"command" [ arg id; atom "="; arg e ]
let cmd_seq c1 c2 = case_v ~var:"command" [ arg c1; atom ";"; arg c2 ]

let cmd_ite e c1 c2 =
  case_v ~var:"command"
    [ atom "IF"; arg e; atom "THEN"; arg c1; atom "ELSE"; arg c2; atom "END" ]

let cmd_while e c =
  case_v ~var:"command" [ atom "WHILE"; arg e; atom "DO"; arg c; atom "END" ]

let fresh_name (ctx : ('a * 'b) list) = Printf.sprintf "x%d" (List.length ctx)
let expr_typ = VarT { synid = "expr" $ no_region; targs = [] } $ no_region
let cmd_typ = VarT { synid = "command" $ no_region; targs = [] } $ no_region

(* ===== Base Impty generator (INT, BOOL only) ===== *)

module Base = struct
  type ty = TInt | TBool
  type ctx = (string * ty) list

  let type_val = function
    | TInt -> case_v ~var:"type" [ atom "INT" ]
    | TBool -> case_v ~var:"type" [ atom "BOOL" ]

  let vars_of ctx ty =
    List.filter_map (fun (name, t) -> if t = ty then Some name else None) ctx

  let expr_typ = VarT { synid = "expr" $ no_region; targs = [] } $ no_region
  let cmd_typ = VarT { synid = "command" $ no_region; targs = [] } $ no_region

  let rec gen_expr (spec : spec) (ctx : ctx) (ty : ty) : t Gen.t =
    let open Gen in
    sized (fun size ->
        let random_expr = Il_gen.gen_of_typ spec expr_typ in
        match ty with
        | TInt ->
            let int_vars = vars_of ctx TInt in
            let base =
              ( 3,
                let* n = choose_int (0, max 1 size) in
                return (expr_num n) )
              ::
              (if int_vars <> [] then
                 [
                   ( 2,
                     let* name = elements int_vars in
                     return (expr_var name) );
                 ]
               else [])
            in
            let recursive =
              if size <= 0 then []
              else
                [
                  ( 1,
                    let* e1 = scale (fun n -> n / 2) (gen_expr spec ctx TInt) in
                    let* e2 = scale (fun n -> n / 2) (gen_expr spec ctx TInt) in
                    return (expr_add e1 e2) );
                ]
            in
            frequency (base @ recursive @ [ (1, random_expr) ])
        | TBool ->
            let bool_vars = vars_of ctx TBool in
            let base =
              ( 3,
                let* b = Arbitrary.Bool.arbitrary in
                return (expr_bool b) )
              ::
              (if bool_vars <> [] then
                 [
                   ( 2,
                     let* name = elements bool_vars in
                     return (expr_var name) );
                 ]
               else [])
            in
            let recursive =
              if size <= 0 then []
              else
                [
                  ( 1,
                    let* e1 = scale (fun n -> n / 2) (gen_expr spec ctx TInt) in
                    let* e2 = scale (fun n -> n / 2) (gen_expr spec ctx TInt) in
                    return (expr_leq e1 e2) );
                  ( 1,
                    let* e = scale (fun n -> n / 2) (gen_expr spec ctx TBool) in
                    return (expr_not e) );
                  ( 1,
                    let* e1 =
                      scale (fun n -> n / 2) (gen_expr spec ctx TBool)
                    in
                    let* e2 =
                      scale (fun n -> n / 2) (gen_expr spec ctx TBool)
                    in
                    return (expr_and e1 e2) );
                ]
            in
            frequency (base @ recursive @ [ (1, random_expr) ]))

  (* Returns the generated command together with the output context, because
   seq must thread the context from c1 into c2. *)
  and gen_command (spec : spec) (ctx : ctx) : (t * ctx) Gen.t =
    let open Gen in
    sized (fun size ->
        let base = [ (1, return (cmd_skip, ctx)) ] in
        let decl =
          let name = fresh_name ctx in
          let* ty = elements [ TInt; TBool ] in
          let* e = gen_expr spec ctx ty in
          return (cmd_decl (type_val ty) (id_val name) e, (name, ty) :: ctx)
        in
        let assign =
          if ctx = [] then []
          else
            [
              ( 1,
                let* name, ty = elements ctx in
                let* e = gen_expr spec ctx ty in
                return (cmd_assign (id_val name) e, ctx) );
            ]
        in
        let recursive =
          if size <= 0 then []
          else
            [
              ( 2,
                let* c1, ctx1 = resize (size / 2) (gen_command spec ctx) in
                let* c2, ctx2 = resize (size / 2) (gen_command spec ctx1) in
                return (cmd_seq c1 c2, ctx2) );
              ( 1,
                let* e = gen_expr spec ctx TBool in
                let* c1, _ = resize (size / 2) (gen_command spec ctx) in
                let* c2, _ = resize (size / 2) (gen_command spec ctx) in
                return (cmd_ite e c1 c2, ctx) );
              ( 1,
                let* e = gen_expr spec ctx TBool in
                let* c, _ = resize (size / 2) (gen_command spec ctx) in
                return (cmd_while e c, ctx) );
            ]
        in
        let random_case =
          ( 1,
            let* cmd = Il_gen.gen_of_typ spec cmd_typ in
            return (cmd, ctx) )
        in
        frequency (base @ [ (3, decl) ] @ assign @ recursive @ [ random_case ]))

  let gen_well_typed_prog (spec : spec) : (string * value) list Gen.t =
    let open Gen in
    let* cmd, _ = gen_command spec [] in
    return [ ("prog", cmd) ]
end

(* ===== Closure Impty generator (INT, BOOL, function types) ===== *)

type closure_ty = CInt | CBool | CFun of closure_ty * closure_ty
type cctx = (string * closure_ty) list

let rec ctype_val = function
  | CInt -> case_v ~var:"type" [ atom "INT" ]
  | CBool -> case_v ~var:"type" [ atom "BOOL" ]
  | CFun (t1, t2) ->
      case_v ~var:"type" [ arg (ctype_val t1); atom "`->"; arg (ctype_val t2) ]

let cvars_of (ctx : cctx) ty =
  List.filter_map (fun (name, t) -> if t = ty then Some name else None) ctx

(* Find variables with type (arg_ty -> ret_ty); return (name, arg_ty) pairs. *)
let cfun_vars_of (ctx : cctx) ret_ty =
  List.filter_map
    (fun (name, t) ->
      match t with
      | CFun (arg_ty, r) when r = ret_ty -> Some (name, arg_ty)
      | _ -> None)
    ctx

let gen_simple_cty : closure_ty Gen.t = Gen.elements [ CInt; CBool ]

(* Generate a random type up to the given nesting depth. *)
let rec gen_cty (depth : int) : closure_ty Gen.t =
  let open Gen in
  if depth <= 0 then gen_simple_cty
  else
    frequency
      [
        (3, gen_simple_cty);
        ( 1,
          let* t1 = gen_cty (depth - 1) in
          let* t2 = gen_cty (depth - 1) in
          return (CFun (t1, t2)) );
      ]

let rec gen_cexpr (spec : spec) (ctx : cctx) (ty : closure_ty) : t Gen.t =
  let open Gen in
  sized (fun size ->
      let random_expr = Il_gen.gen_of_typ spec expr_typ in
      match ty with
      | CInt ->
          let ivars = cvars_of ctx CInt in
          let fvars = cfun_vars_of ctx CInt in
          let base =
            [
              ( 3,
                let* n = choose_int (0, max 1 size) in
                return (expr_num n) );
            ]
            @
            if ivars <> [] then
              [
                ( 2,
                  let* v = elements ivars in
                  return (expr_var v) );
              ]
            else []
          in
          let recursive =
            if size <= 0 then []
            else
              [
                ( 1,
                  let* e1 = scale (fun n -> n / 2) (gen_cexpr spec ctx CInt) in
                  let* e2 = scale (fun n -> n / 2) (gen_cexpr spec ctx CInt) in
                  return (expr_add e1 e2) );
                ( 1,
                  let* ta = gen_simple_cty in
                  let* ef =
                    scale
                      (fun n -> n / 2)
                      (gen_cexpr spec ctx (CFun (ta, CInt)))
                  in
                  let* ea = scale (fun n -> n / 2) (gen_cexpr spec ctx ta) in
                  return (expr_call ef ea) );
              ]
              @
              if fvars <> [] then
                [
                  ( 2,
                    let* fname, at = elements fvars in
                    let* ea = scale (fun n -> n / 2) (gen_cexpr spec ctx at) in
                    return (expr_call (expr_var fname) ea) );
                ]
              else []
          in
          frequency (base @ recursive @ [ (1, random_expr) ])
      | CBool ->
          let bvars = cvars_of ctx CBool in
          let fvars = cfun_vars_of ctx CBool in
          let base =
            [
              ( 3,
                let* b = Arbitrary.Bool.arbitrary in
                return (expr_bool b) );
            ]
            @
            if bvars <> [] then
              [
                ( 2,
                  let* v = elements bvars in
                  return (expr_var v) );
              ]
            else []
          in
          let recursive =
            if size <= 0 then []
            else
              [
                ( 1,
                  let* e1 = scale (fun n -> n / 2) (gen_cexpr spec ctx CInt) in
                  let* e2 = scale (fun n -> n / 2) (gen_cexpr spec ctx CInt) in
                  return (expr_leq e1 e2) );
                ( 1,
                  let* e = scale (fun n -> n / 2) (gen_cexpr spec ctx CBool) in
                  return (expr_not e) );
                ( 1,
                  let* e1 = scale (fun n -> n / 2) (gen_cexpr spec ctx CBool) in
                  let* e2 = scale (fun n -> n / 2) (gen_cexpr spec ctx CBool) in
                  return (expr_and e1 e2) );
                ( 1,
                  let* ta = gen_simple_cty in
                  let* ef =
                    scale
                      (fun n -> n / 2)
                      (gen_cexpr spec ctx (CFun (ta, CBool)))
                  in
                  let* ea = scale (fun n -> n / 2) (gen_cexpr spec ctx ta) in
                  return (expr_call ef ea) );
              ]
              @
              if fvars <> [] then
                [
                  ( 2,
                    let* fname, at = elements fvars in
                    let* ea = scale (fun n -> n / 2) (gen_cexpr spec ctx at) in
                    return (expr_call (expr_var fname) ea) );
                ]
              else []
          in
          frequency (base @ recursive @ [ (1, random_expr) ])
      | CFun (ta, tr) ->
          let fvars = cvars_of ctx ty in
          (* variables x : CFun(X, CFun(ta,tr)) — calling x(arg) yields CFun(ta,tr) *)
          let hof_vars = cfun_vars_of ctx ty in
          (* Use "p" prefix so params never collide with decl names ("x" prefix).
         Without this, f(a)(b) can still find the param in EC via the function binding,
         so the env_clo bug would not cause a lookup failure. *)
          let param = Printf.sprintf "p%d" (List.length ctx) in
          let ext_ctx = (param, ta) :: ctx in
          let base =
            [
              ( 4,
                let* body =
                  scale (fun n -> n / 2) (gen_cexpr spec ext_ctx tr)
                in
                return
                  (expr_fun (ctype_val ta) (id_val param) (ctype_val tr) body)
              );
            ]
            @ (if fvars <> [] then
                 [
                   ( 2,
                     let* v = elements fvars in
                     return (expr_var v) );
                 ]
               else [])
            @ [ (1, random_expr) ]
          in
          let recursive =
            if size <= 0 then []
            else if hof_vars <> [] then
              [
                ( 1,
                  let* fname, at = elements hof_vars in
                  let* ea = scale (fun n -> n / 2) (gen_cexpr spec ctx at) in
                  return (expr_call (expr_var fname) ea) );
              ]
            else []
          in
          frequency (base @ recursive))

and gen_ccommand (spec : spec) (ctx : cctx) : (t * cctx) Gen.t =
  let open Gen in
  sized (fun size ->
      let base = [ (1, return (cmd_skip, ctx)) ] in
      let decl =
        let name = fresh_name ctx in
        let* ty = gen_cty 1 in
        let* e = gen_cexpr spec ctx ty in
        return (cmd_decl (ctype_val ty) (id_val name) e, (name, ty) :: ctx)
      in
      let assign =
        if ctx = [] then []
        else
          [
            ( 2,
              let* name, ty = elements ctx in
              let* e = gen_cexpr spec ctx ty in
              return (cmd_assign (id_val name) e, ctx) );
          ]
      in
      let partial_app =
        let cfun_funs =
          List.filter_map
            (fun (name, t) ->
              match t with
              | CFun (at, (CFun _ as ret_ty)) -> Some (name, at, ret_ty)
              | _ -> None)
            ctx
        in
        if cfun_funs = [] then []
        else
          let name = fresh_name ctx in
          [
            ( 3,
              let* fname, at, ret_ty = elements cfun_funs in
              let* arg = gen_cexpr spec ctx at in
              return
                ( cmd_decl (ctype_val ret_ty) (id_val name)
                    (expr_call (expr_var fname) arg),
                  (name, ret_ty) :: ctx ) );
          ]
      in
      let recursive =
        if size <= 0 then []
        else
          [
            ( 2,
              let* c1, ctx1 = resize (size / 2) (gen_ccommand spec ctx) in
              let* c2, ctx2 = resize (size / 2) (gen_ccommand spec ctx1) in
              return (cmd_seq c1 c2, ctx2) );
            ( 1,
              let* e = gen_cexpr spec ctx CBool in
              let* c1, _ = resize (size / 2) (gen_ccommand spec ctx) in
              let* c2, _ = resize (size / 2) (gen_ccommand spec ctx) in
              return (cmd_ite e c1 c2, ctx) );
            ( 1,
              let* e = gen_cexpr spec ctx CBool in
              let* c, _ = resize (size / 2) (gen_ccommand spec ctx) in
              return (cmd_while e c, ctx) );
          ]
      in
      let random_case =
        ( 1,
          let* cmd = Il_gen.gen_of_typ spec cmd_typ in
          return (cmd, ctx) )
      in
      frequency
        (base
        @ [ (3, decl) ]
        @ partial_app @ assign @ recursive @ [ random_case ]))

let gen_functions_prog (spec : spec) : (string * value) list Gen.t =
  let open Gen in
  let* cmd, _ = gen_ccommand spec [] in
  return [ ("prog", cmd) ]

(* Generates programs that specifically exercise curried closure calls, designed
   to expose the env_clo bug in Eval_expr/call.  The pattern is always:
     f : at -> (bt -> at) = FUN (at p0) -> bt->at { FUN (bt p1) -> at { p0 } }
     r : at               = f(arg_a)(arg_b)
   With the buggy rule the second call evaluates the body `p0` in the call-site
   env (which never contains p0), causing a lookup failure. *)
let gen_curried_prog (spec : spec) : (string * value) list Gen.t =
  let open Gen in
  let ctx = [] in
  let p_outer = Printf.sprintf "p%d" (List.length ctx) in
  let p_inner = Printf.sprintf "p%d" (List.length ctx + 1) in
  let name_f = fresh_name ctx in
  let name_r = fresh_name ((name_f, CInt) :: ctx) in
  let* at = gen_simple_cty in
  let* bt = gen_simple_cty in
  let fun_ty = CFun (at, CFun (bt, at)) in
  let ctx_f = (name_f, fun_ty) :: ctx in
  let ctx_r = (name_r, at) :: ctx_f in
  let inner_fun =
    expr_fun (ctype_val bt) (id_val p_inner) (ctype_val at) (expr_var p_outer)
  in
  let outer_fun =
    expr_fun (ctype_val at) (id_val p_outer)
      (ctype_val (CFun (bt, at)))
      inner_fun
  in
  let* arg_a = gen_cexpr spec ctx_f at in
  let* arg_b = gen_cexpr spec ctx_f bt in
  let cmd_f = cmd_decl (ctype_val fun_ty) (id_val name_f) outer_fun in
  let cmd_r =
    cmd_decl (ctype_val at) (id_val name_r)
      (expr_call (expr_call (expr_var name_f) arg_a) arg_b)
  in
  ignore ctx_r;
  return [ ("prog", cmd_seq cmd_f cmd_r) ]

let manual_gens : (string * (spec -> (string * value) list Gen.t)) list =
  [
    ("base_prog", Base.gen_well_typed_prog);
    ("functions_prog", gen_functions_prog);
    ("curried_prog", gen_curried_prog);
  ]
