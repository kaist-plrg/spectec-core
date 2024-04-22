open Domain
open Utils

let indent depth = Print.print_indent depth

let rec print_value (value : value) =
  match value with
  | VBool value -> Printf.sprintf "%b" value
  | VAInt value -> Printf.sprintf "%s" (Bigint.to_string value)
  | VInt { value; width } ->
      Printf.sprintf "%ss%s" (Bigint.to_string width) (Bigint.to_string value)
  | VBit { value; width } ->
      Printf.sprintf "%sw%s" (Bigint.to_string width) (Bigint.to_string value)
  | VString value -> Printf.sprintf "\"%s\"" value
  | VError -> "error"
  | VTuple values ->
      Printf.sprintf "(%s)" (String.concat ", " (List.map print_value values))
  | VStruct { entries } ->
      Printf.sprintf "{%s}"
        (String.concat ", "
           (List.map
              (fun (key, value) ->
                Printf.sprintf "%s: %s" key (print_value value))
              entries))
  | VHeader { valid; entries } ->
      Printf.sprintf "Header(%b, {%s})" valid
        (String.concat ", "
           (List.map
              (fun (key, value) ->
                Printf.sprintf "%s: %s" key (print_value value))
              entries))
  | VRef rvalue -> Printf.sprintf "*%s" (String.concat "." rvalue)

and print_obj (obj : obj) =
  let depth = 1 in
  match obj with
  | OPackage { genv; tsto; vsto; _ } ->
      Printf.sprintf "Package {\n%sgenv = %s\n%ststo = %s\n%svsto = %s }"
        (indent (depth + 2))
        (print_env genv)
        (indent (depth + 2))
        (print_tsto tsto)
        (indent (depth + 2))
        (print_vsto vsto)
  | OParser { tsto; vsto; funcs; _ } ->
      Printf.sprintf "Parser {\n%ststo = %s\n%svsto = %s\n%sfuncs =\n%s }"
        (indent (depth + 2))
        (print_tsto tsto)
        (indent (depth + 2))
        (print_vsto vsto)
        (indent (depth + 2))
        (String.concat "\n" (List.map print_func funcs))
  | OControl { tsto; vsto; funcs; _ } ->
      Printf.sprintf "Control {\n%ststo = %s\n%svsto = %s\n%sfuncs =\n%s }"
        (indent (depth + 2))
        (print_tsto tsto)
        (indent (depth + 2))
        (print_vsto vsto)
        (indent (depth + 2))
        (String.concat "\n" (List.map print_func funcs))
  | OExtern { tsto; vsto; _ } ->
      Printf.sprintf "Extern {\n%ststo = %s\n%svsto = %s }"
        (indent (depth + 2))
        (print_tsto tsto)
        (indent (depth + 2))
        (print_vsto vsto)
  | OTable { genv; lenv; _ } ->
      Printf.sprintf "Table {\n%sgenv = %s\n%slenv = %s }"
        (indent (depth + 2))
        (print_env genv)
        (indent (depth + 2))
        (print_env lenv)
  | OFunction -> Printf.sprintf "Function"
  | OValueSet -> Printf.sprintf "ValueSet"

and print_typ (typ : typ) =
  match typ with
  | TBool -> "bool"
  | TAInt -> "int"
  | TInt { width } -> Printf.sprintf "int<%s>" (Bigint.to_string width)
  | TBit { width } -> Printf.sprintf "bit<%s>" (Bigint.to_string width)
  | TVBit { width } -> Printf.sprintf "varbit<%s>" (Bigint.to_string width)
  | TArray { typ; size } ->
      Printf.sprintf "array<%s>[%s]" (print_typ typ) (Bigint.to_string size)
  | TString -> "string"
  | TError -> "error"
  | TTuple typs ->
      Printf.sprintf "(%s)" (String.concat ", " (List.map print_typ typs))
  | TEnum { entries } -> Printf.sprintf "enum{%s}" (String.concat ", " entries)
  | TSEnum { typ; entries } ->
      Printf.sprintf "enum<%s>{%s}" (print_typ typ) (String.concat ", " entries)
  | THeader { entries } ->
      Printf.sprintf "header{%s}"
        (String.concat ", "
           (List.map
              (fun (key, typ) -> Printf.sprintf "%s: %s" key (print_typ typ))
              entries))
  | TUnion { entries } ->
      Printf.sprintf "union{%s}"
        (String.concat ", "
           (List.map
              (fun (key, typ) -> Printf.sprintf "%s: %s" key (print_typ typ))
              entries))
  | TStruct { entries } ->
      Printf.sprintf "struct{%s}"
        (String.concat ", "
           (List.map
              (fun (key, typ) -> Printf.sprintf "%s: %s" key (print_typ typ))
              entries))
  | TName { name } -> name
  | TNewType { name } -> name
  | TRef -> "ref"

and print_func (func : func) =
  let depth = 4 in
  match func with
  | FNormal { name; genv; lenv; _ } | FParser { name; genv; lenv; _ } ->
      Printf.sprintf "%sFunction %s {\n%sgenv = %s\n%slenv = %s }"
        (indent depth) name
        (indent (depth + 2))
        (print_env genv)
        (indent (depth + 2))
        (print_env lenv)
  | FExtern { name; _ } ->
      Printf.sprintf "%sFunction %s { extern }" (indent depth) name

and print_cclos (_cclos : cclos) = "(TODO) print_cclos"

and print_env (env : env) =
  let bindings =
    Env.bindings env
    |> List.sort (fun (_, loc) (_, loc') -> Loc.compare loc loc')
    |> List.fold_left
         (fun acc (key, loc) ->
           acc @ [ Printf.sprintf "%s : %s" key (Loc.print loc) ])
         []
  in
  "{ " ^ String.concat ", " bindings ^ " }"

and print_tsto (tsto : tsto) =
  let bindings =
    Heap.bindings tsto
    |> List.sort (fun (loc, _) (loc', _) -> Loc.compare loc loc')
    |> List.fold_left
         (fun acc (loc, typ) ->
           acc @ [ Printf.sprintf "%s : %s" (Loc.print loc) (print_typ typ) ])
         []
  in
  "{ " ^ String.concat ", " bindings ^ " }"

and print_vsto (vsto : vsto) =
  let bindings =
    Heap.bindings vsto
    |> List.sort (fun (loc, _) (loc', _) -> Loc.compare loc loc')
    |> List.fold_left
         (fun acc (loc, value) ->
           acc
           @ [ Printf.sprintf "%s : %s" (Loc.print loc) (print_value value) ])
         []
  in
  "{ " ^ String.concat ", " bindings ^ " }"
