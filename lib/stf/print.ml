open Ast
module F = Format

let print_option fmt printer = function Some s -> printer fmt s | None -> ()
let print_int fmt i = F.fprintf fmt "%d" i
let print_string fmt s = F.fprintf fmt "%s" s
let print_name fmt name = print_string fmt name
let print_id fmt id = print_string fmt id
let print_number fmt number = print_string fmt number
let print_port fmt port = print_string fmt port
let print_packet fmt packet = print_string fmt packet
let print_expect fmt expect = print_string fmt expect

let print_arg fmt arg =
  let id, number = arg in
  F.fprintf fmt "(%a, %a)" print_id id print_number number

let print_action fmt action =
  let name, args = action in
  F.fprintf fmt "%a(%a)" print_name name
    (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt ", ") print_arg)
    args

let print_mtchkind fmt = function
  | Num number -> print_number fmt number
  | Slash (number_l, number_r) ->
      F.fprintf fmt "%a/%a" print_number number_l print_number number_r

let print_mtch fmt mtch =
  let name, mtchkind = mtch in
  F.fprintf fmt "%a(%a)" print_name name print_mtchkind mtchkind

let print_id_or_index fmt = function
  | Id id -> print_id fmt id
  | Index number -> print_number fmt number

let print_cond fmt = function
  | Eq -> print_string fmt "=="
  | Ne -> print_string fmt "!="
  | Le -> print_string fmt "<="
  | Lt -> print_string fmt "<"
  | Ge -> print_string fmt ">="
  | Gt -> print_string fmt ">"

let print_ctr fmt = function
  | Bytes -> print_string fmt "bytes"
  | Packets -> print_string fmt "packets"

let print_stmt fmt = function
  | Wait -> print_string fmt "wait"
  | RemoveAll -> print_string fmt "remove_all"
  | Expect (port, expect_opt) ->
      F.fprintf fmt "expect %a %a" print_port port
        (F.pp_print_option print_expect)
        expect_opt
  | Packet (port, packet) ->
      F.fprintf fmt "packet %a %a" print_port port print_packet packet
  | NoPacket -> print_string fmt "no_packet"
  | Add (name, int_opt, mtchs, action, id_opt) ->
      F.fprintf fmt "add %a %a %a %a %a" print_name name
        (F.pp_print_option print_int)
        int_opt
        (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt " ") print_mtch)
        mtchs print_action action
        (F.pp_print_option print_id)
        id_opt
  | SetDefault (name, action) ->
      F.fprintf fmt "setdefault %a %a" print_name name print_action action
  | CheckCounter (id, id_or_index, (ctr, cond, number)) ->
      F.fprintf fmt "check_counter %a(%a) %a %a %a" print_id id
        print_id_or_index id_or_index
        (F.pp_print_option print_ctr)
        ctr print_cond cond print_number number

let print_stmts fmt stmts =
  F.pp_print_list
    ~pp_sep:(fun fmt () -> F.fprintf fmt ";@ ")
    print_stmt fmt stmts
