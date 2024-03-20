open Utils

(* Values *)

type base =
  | Bool of bool
  | AInt of Bigint.t
  | Int of { value : Bigint.t; width : Bigint.t }
  | Bit of { value : Bigint.t; width : Bigint.t }
  | String of string
  | Tuple of base list
  | Struct of { entries : (string * base) list }
  | Header of { valid : bool; entries : (string * base) list }

type t = Base of base | Ref of Path.t

(* Printers *)

let rec print_base (bvalue : base) =
  match bvalue with
  | Bool value -> Printf.sprintf "%b" value
  | AInt value -> Printf.sprintf "%s" (Bigint.to_string value)
  | Int { value; width } ->
      Printf.sprintf "%ss%s" (Bigint.to_string width) (Bigint.to_string value)
  | Bit { value; width } ->
      Printf.sprintf "%sw%s" (Bigint.to_string width) (Bigint.to_string value)
  | String value -> Printf.sprintf "\"%s\"" value
  | Tuple values ->
      Printf.sprintf "(%s)" (String.concat ", " (List.map print_base values))
  | Struct { entries } ->
      Printf.sprintf "{%s}"
        (String.concat ", "
           (List.map
              (fun (key, value) ->
                Printf.sprintf "%s: %s" key (print_base value))
              entries))
  | Header { valid; entries } ->
      Printf.sprintf "Header(%b, {%s})" valid
        (String.concat ", "
           (List.map
              (fun (key, value) ->
                Printf.sprintf "%s: %s" key (print_base value))
              entries))

let print (t : t) =
  match t with
  | Base bvalue -> print_base bvalue
  | Ref rvalue -> Printf.sprintf "Ref(%s)" (String.concat "." rvalue)

(* Utils *)

let extract_base (value : t) : base =
  match value with
  | Base bvalue -> bvalue
  | _ -> Printf.sprintf "Not a base value: %s" (print value) |> failwith

let extract_bigint (value : t) : Bigint.t =
  let bvalue = extract_base value in
  match bvalue with
  | AInt value -> value
  | Int { value; _ } -> value
  | Bit { value; _ } -> value
  | _ ->
      Printf.sprintf "Not a int/bit value: %s" (print_base bvalue) |> failwith
