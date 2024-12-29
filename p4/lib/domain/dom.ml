open Util.Source
open Util.Error

let check = check_checker
let error_no_info = error_checker_no_info

(* Variable identifiers *)

module Id = struct
  type t = string

  let pp fmt t = Format.fprintf fmt "%s" t
  let compare = compare
end

module IdSet = struct
  include Set.Make (Id)

  let pp fmt s =
    let pp_id fmt id = Format.fprintf fmt "%a" Id.pp id in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_id) (elements s)

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module IdMap = struct
  include Map.Make (Id)

  let keys m = List.map fst (bindings m)
  let values m = List.map snd (bindings m)

  let pp pp_v fmt m =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a : %a" Id.pp k pp_v v in
    let bindings = bindings m in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Type identifiers *)

module TId = Id
module TIdSet = IdSet
module TIdMap = IdMap

(* Function identifiers *)

module FId = struct
  type t = name * param list
  and name = string
  and param = string * bool

  let pp_name fmt name = Format.fprintf fmt "%s" name
  let compare = compare
  let pp_param fmt (id, _) = Format.fprintf fmt "%s" id

  let pp fmt (name, params) =
    Format.fprintf fmt "%a(%a)" pp_name name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
         pp_param)
      params

  let to_fid (id : El.Ast.id) (params : El.Ast.param list) =
    let params =
      List.map
        (fun param ->
          let id, _, _, value_default, _ = param.it in
          (id.it, Option.is_some value_default))
        params
    in
    (id.it, params)

  let to_names (args : El.Ast.arg list) =
    List.map
      (fun arg ->
        match arg.it with Lang.Ast.NameA (id, _) -> Some id.it | _ -> None)
      args
end

module FIdSet = struct
  include Set.Make (FId)

  let eq = equal

  let pp fmt s =
    let pp_fid fmt fid = Format.fprintf fmt "%a" FId.pp fid in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_fid) (elements s)

  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module FIdMap = struct
  include Map.Make (FId)

  let keys m = List.map fst (bindings m)
  let values m = List.map snd (bindings m)

  let pp pp_v fmt m =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a : %a" FId.pp k pp_v v in
    let bindings = bindings m in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Constructor identifiers *)

module CId = FId
module CIdSet = FIdSet
module CIdMap = FIdMap

(* Object identifiers *)

module OId = struct
  type t = Id.t list

  let pp fmt t = String.concat "." t |> Format.fprintf fmt "%s"
  let compare = compare
end

module OIdSet = struct
  include Set.Make (OId)

  let pp fmt s =
    let pp_oid fmt oid = Format.fprintf fmt "%a" OId.pp oid in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_oid) (elements s)

  let eq = equal
  let of_list l = List.fold_left (fun acc x -> add x acc) empty l
end

module OIdMap = struct
  include Map.Make (OId)

  let keys m = List.map fst (bindings m)
  let values m = List.map snd (bindings m)

  let pp pp_v fmt m =
    let pp_binding fmt (k, v) = Format.fprintf fmt "%a : %a" OId.pp k pp_v v in
    let bindings = bindings m in
    Format.fprintf fmt "{ %a }" (Format.pp_print_list pp_binding) bindings

  let diff m_a m_b =
    let keys_a = keys m_a in
    let keys_b = keys m_b in
    let keys_diff = List.filter (fun k -> not (List.mem k keys_b)) keys_a in
    List.fold_left (fun acc k -> add k (find k m_a) acc) empty keys_diff

  let subset eq_v m_a m_b =
    List.for_all
      (fun (k, v_a) ->
        match find_opt k m_b with Some v_b -> eq_v v_a v_b | None -> false)
      (bindings m_a)

  let eq eq_v m_a m_b = subset eq_v m_a m_b && subset eq_v m_b m_a
  let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
end

(* Environment functor *)

module MakeIdEnv (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  include IdMap

  type t = V.t IdMap.t

  let pp fmt env = IdMap.pp V.pp fmt env

  let find id env =
    match find_opt id env with
    | Some value -> value
    | None -> Format.asprintf "key not found: %a" Id.pp id |> error_no_info

  let add_nodup id value env =
    if mem id env then
      Format.asprintf "key already exists: %a" Id.pp id |> error_no_info
    else add id value env
end

module MakeTIdEnv = MakeIdEnv

module MakeFIdEnv (V : sig
  type t

  val pp : Format.formatter -> t -> unit
  val eq_kind : t -> t -> bool
end) =
struct
  include FIdMap

  type t = V.t FIdMap.t

  let pp fmt env = FIdMap.pp V.pp fmt env

  (* Lookup for matching def site to def site *)
  (* (TODO) This must also impose the restriction that default values must be coherent, if specified *)

  let find_opt (fid : FId.t) fenv = List.assoc_opt fid (bindings fenv)

  let find (fid : FId.t) fenv =
    match find_opt fid fenv with
    | Some func -> func
    | None -> Format.asprintf "key not found: %a" FId.pp fid |> error_no_info

  (* Lookups for matching call site to def site *)

  (* Overloaded lookup, allowing defaults *)

  let check_named_args args =
    check
      (List.for_all Option.is_some args || List.for_all Option.is_none args)
      "(check_named_args) either all or no arguments must specify the \
       parameter name"

  (* (6.8.1) Justification

     Following is a summary of the constraints imposed by the parameter directions:

      - If parameters with default values do not appear at the end of the list of parameters,
        invocations that use the default values must use named arguments. *)

  (* (8.20) Method invocations and function calls

     A function call or method invocation can optionally specify for each argument
     the corresponding parameter name.
     It is illegal to use names only for some arguments:
     either all or no arguments must specify the parameter name. *)

  let check_func_name fname fname' = fname = fname'
  let check_arity_more args params = List.length args > List.length params
  let check_arity args params = List.length args = List.length params
  let check_args_named arg_names = arg_names <> []

  let find_match_named func arg_names params =
    let param_names = List.map fst params in
    let param_names = List.sort String.compare param_names in
    if List.for_all2 ( = ) arg_names param_names then Some (func, []) else None

  let find_match_named_default func arg_names params =
    let param_names = List.map fst params in
    let param_names = List.sort String.compare param_names in
    let param_missing_names =
      List.filter
        (fun param_name -> not (List.mem param_name arg_names))
        param_names
    in
    let arg_names =
      arg_names @ param_missing_names |> List.sort String.compare
    in
    find_match_named func arg_names params
    |> Option.map (fun (func, _) -> (func, param_missing_names))

  let find_match_unnamed_default func args params =
    let params_default =
      List.filteri (fun i _ -> i >= List.length args) params
    in
    let params_names, params_default = List.split params_default in
    if List.for_all Fun.id params_default then Some (func, params_names)
    else None

  let find_overloaded_opt (fname, args) fenv =
    check_named_args args;
    let arg_names =
      if List.for_all Option.is_some args then
        List.map Option.get args |> List.sort String.compare
      else []
    in
    let funcs =
      List.filter_map
        (fun ((fname', params), func) ->
          (* Falls into roughly five cases:
             (1) Name mismatch or more args than params
             (2) Arity match
                (a) Named args (b) Unnamed args
             (3) Arity mismatch (maybe due to default param)
                (a) Named args (b) Unnamed args *)
          if not (check_func_name fname fname') then None
          else if check_arity_more args params then None
          else if check_arity args params then
            if check_args_named arg_names then
              find_match_named func arg_names params
            else Some (func, [])
          else if check_args_named arg_names then
            find_match_named_default func arg_names params
          else find_match_unnamed_default func args params)
        (bindings fenv)
    in
    match funcs with
    | [] -> None
    | [ func ] -> Some func
    | _ ->
        Format.asprintf
          "(find_overloaded_opt) cannot resolve overloaded function given %a"
          FId.pp_name fname
        |> error_no_info

  let find_overloaded (fname, args) fenv =
    match find_overloaded_opt (fname, args) fenv with
    | Some value -> value
    | _ ->
        Format.asprintf "key not found: %a" FId.pp_name fname |> error_no_info

  (* Non-overloaded lookup, allowing defaults *)

  let find_non_overloaded_opt' fname fenv =
    List.filter (fun ((fname', _), _) -> fname = fname') (bindings fenv)
    |> List.map snd

  let find_non_overloaded_opt (fname, args) fenv =
    check_named_args args;
    let funcs = find_non_overloaded_opt' fname fenv in
    match funcs with [] -> None | [ func ] -> Some func | _ -> assert false

  let find_non_overloaded (fname, args) fenv =
    match find_non_overloaded_opt (fname, args) fenv with
    | Some value -> value
    | _ ->
        Format.asprintf "key not found: %a" FId.pp_name fname |> error_no_info

  (* Adders *)

  let add_nodup_overloaded fid value fenv =
    if mem fid fenv then
      Format.asprintf "key already exists: %a" FId.pp fid |> error_no_info
    else
      let fname, _ = fid in
      match find_non_overloaded_opt' fname fenv with
      | [] -> add fid value fenv
      | values ->
          if not (List.for_all (V.eq_kind value) values) then
            Format.asprintf "key already exists: %a" FId.pp fid |> error_no_info
          else add fid value fenv

  let add_nodup_non_overloaded fid value fenv =
    let fname, _ = fid in
    match find_non_overloaded_opt' fname fenv with
    | [] -> add fid value fenv
    | _ -> Format.asprintf "key already exists: %a" FId.pp fid |> error_no_info
end

module MakeCIdEnv = MakeFIdEnv

module MakeOIdEnv (V : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  include OIdMap

  type t = V.t OIdMap.t

  let pp fmt env = OIdMap.pp V.pp fmt env

  let find id env =
    match find_opt id env with
    | Some value -> value
    | None -> Format.asprintf "key not found: %a" OId.pp id |> error_no_info

  let add_nodup id value env =
    if mem id env then
      Format.asprintf "key already exists: %a" OId.pp id |> error_no_info
    else add id value env
end

(* Pair functor *)

module MakePair (A : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (B : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = A.t * B.t

  let pp fmt (a, b) = Format.fprintf fmt "(%a, %a)" A.pp a B.pp b
end

(* Triple functor *)

module MakeTriple (A : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (B : sig
  type t

  val pp : Format.formatter -> t -> unit
end) (C : sig
  type t

  val pp : Format.formatter -> t -> unit
end) =
struct
  type t = A.t * B.t * C.t

  let pp fmt (a, b, c) = Format.fprintf fmt "(%a, %a, %a)" A.pp a B.pp b C.pp c
end
