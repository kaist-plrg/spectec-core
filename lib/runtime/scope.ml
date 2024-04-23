open Domain

(* Finders *)

let find_var_top (name : string) (benv : benv) =
  let genv, _, tsto, vsto = benv in
  match Utils.Scope.find_double name genv tsto vsto with
  | Some (typ, value) -> (typ, value)
  | _ -> Printf.sprintf "(find_var_top) %s not found" name |> failwith

let find_var (name : string) (benv : benv) =
  let _, lenv, tsto, vsto = benv in
  match Utils.Scope.find_double name lenv tsto vsto with
  | Some (typ, value) -> (typ, value)
  | _ -> find_var_top name benv

(* Adders *)

let add_var = Utils.Scope.add_double

let add_var_without_value = Utils.Scope.add_single

(* Updaters *)

let update_value (name : string) (value : value) (benv : benv) =
  let genv, lenv, tsto, vsto = benv in
  let vsto = Utils.Scope.update name value lenv vsto in
  (genv, lenv, tsto, vsto)
