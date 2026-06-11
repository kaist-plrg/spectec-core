open Lang.Il
open Value
open Case

let int_type = case_v ~var:"type" [ kw "INT" ]
let bool_type = case_v ~var:"type" [ kw "BOOL" ]
let id_val name = case_v ~var:"id" [ tag "ID"; arg (text name) ]
let cmd_decl ty id e = case_v ~var:"command" [ arg ty; arg id; op "="; arg e ]
let cmd_seq c1 c2 = case_v ~var:"command" [ arg c1; op ";"; arg c2 ]

let name_of_id (v : Value.t) : string option =
  match flatten_case_v v with
  | _, [ "_ID" ], [ s ] -> Some (get_text s)
  | _ -> None

let bindings_of_map (m : Value.t) : (string * Value.t) list option =
  match m.it with
  | ListV pairs ->
      List.fold_right
        (fun pair acc ->
          match (acc, flatten_case_v pair) with
          | Some acc, (_, [ "->" ], [ k; v ]) ->
              Option.map (fun name -> (name, v) :: acc) (name_of_id k)
          | _ -> None)
        pairs (Some [])
  | _ -> None

(* A bare literal or id always agrees with its type, so only compound
   expressions occur as counterexamples. *)
let wrapper_type (expr : Value.t) : Value.t option =
  match flatten_case_v expr with
  | _, [ "+" ], _ -> Some int_type
  | _, [ "<=" ], _ | _, [ "&&" ], _ | _, [ "!" ], _ -> Some bool_type
  | _ -> None

let prog_of_env (bindings : (id' * Value.t) list) : Value.t option =
  let ( let* ) = Option.bind in
  let* env = List.assoc_opt "env" bindings in
  let* tenv = List.assoc_opt "tenv" bindings in
  let* expr = List.assoc_opt "expr" bindings in
  let* env_map = bindings_of_map env in
  let* tenv_map = bindings_of_map tenv in
  let* wty = wrapper_type expr in
  let* decls =
    List.fold_right
      (fun (name, ty) acc ->
        let* acc = acc in
        let* value = List.assoc_opt name env_map in
        Some (cmd_decl ty (id_val name) value :: acc))
      tenv_map (Some [])
  in
  let wrapper = cmd_decl wty (id_val "result") expr in
  Some (List.fold_right cmd_seq decls wrapper)
