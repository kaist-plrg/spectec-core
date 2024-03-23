open Utils

(* Values *)

type t =
  | Bool of bool
  | AInt of Bigint.t
  | Int of { value : Bigint.t; width : Bigint.t }
  | Bit of { value : Bigint.t; width : Bigint.t }
  | String of string
  | Tuple of t list
  | Struct of { entries : (string * t) list }
  | Header of { valid : bool; entries : (string * t) list }
  | Ref of Path.t

(* Printers *)

let rec print (t : t) =
  match t with
  | Bool value -> Printf.sprintf "%b" value
  | AInt value -> Printf.sprintf "%s" (Bigint.to_string value)
  | Int { value; width } ->
      Printf.sprintf "%ss%s" (Bigint.to_string width) (Bigint.to_string value)
  | Bit { value; width } ->
      Printf.sprintf "%sw%s" (Bigint.to_string width) (Bigint.to_string value)
  | String value -> Printf.sprintf "\"%s\"" value
  | Tuple values ->
      Printf.sprintf "(%s)" (String.concat ", " (List.map print values))
  | Struct { entries } ->
      Printf.sprintf "{%s}"
        (String.concat ", "
           (List.map
              (fun (key, value) -> Printf.sprintf "%s: %s" key (print value))
              entries))
  | Header { valid; entries } ->
      Printf.sprintf "Header(%b, {%s})" valid
        (String.concat ", "
           (List.map
              (fun (key, value) -> Printf.sprintf "%s: %s" key (print value))
              entries))
  | Ref rvalue -> Printf.sprintf "*%s" (String.concat "." rvalue)

(* Utils *)

let extract_bigint (t : t) : Bigint.t =
  match t with
  | AInt value -> value
  | Int { value; _ } -> value
  | Bit { value; _ } -> value
  | _ -> Printf.sprintf "Not a int/bit value: %s" (print t) |> failwith
