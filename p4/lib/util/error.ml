open Source

exception ParseErr of string * info
exception CheckErr of string * info
exception InstErr of string * info
exception InterpErr of string * info
exception DriverErr of string
exception StfErr of string

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

let error_inst_pass_info (info : info) = function
  | InstErr (msg, M "") -> raise (InstErr (msg, info))
  | err -> raise err

let error_interp_info (info : info) (msg : string) =
  raise (InterpErr (msg, info))

let error_interp_no_info (msg : string) = raise (InterpErr (msg, no_info))

let error_interp_pass_info (info : info) = function
  | InterpErr (msg, M "") -> raise (InterpErr (msg, info))
  | err -> raise err

let error_driver (msg : string) = raise (DriverErr msg)
let error_stf (msg : string) = raise (StfErr msg)

(* Conditionals *)

let check_checker (b : bool) (msg : string) : unit =
  if not b then error_checker_no_info msg

let check_inst (b : bool) (msg : string) : unit =
  if not b then error_inst_no_info msg

let check_interp (b : bool) (msg : string) : unit =
  if not b then error_interp_no_info msg

let check_driver (b : bool) (msg : string) : unit =
  if not b then error_driver msg

let implies (p : bool) (q : bool) : bool = (not p) || q
