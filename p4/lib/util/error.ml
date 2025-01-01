open Source

exception ParseErr of string * info
exception CheckErr of string * info
exception InstErr of string * info

(* Constructors *)

let error_parser_info (info : info) (msg : string) =
  raise (ParseErr (msg, info))

let error_parser_no_info (msg : string) = raise (ParseErr (msg, no_info))

let error_checker_info (info : info) (msg : string) =
  raise (CheckErr (msg, info))

let error_checker_no_info (msg : string) = raise (CheckErr (msg, no_info))

let error_checker_pass_info (info : info) = function
  | CheckErr (msg, M "") -> raise (CheckErr (msg, info))
  | err -> raise err

let error_inst_info (info : info) (msg : string) = raise (InstErr (msg, info))
let error_inst_no_info (msg : string) = raise (InstErr (msg, no_info))

(* Conditionals *)

let check_checker (b : bool) (msg : string) : unit =
  if not b then error_checker_no_info msg

let check_inst (b : bool) (msg : string) : unit =
  if not b then error_inst_no_info msg

let implies (p : bool) (q : bool) : bool = (not p) || q
