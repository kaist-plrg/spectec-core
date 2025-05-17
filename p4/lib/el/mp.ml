module L = Lang.Ast
module P = Lang.Pp
open Ast
open Util.Pp
open Util.Source

(* Utils *)

let mp_it ~mp ~level it = Printf.sprintf "((%s) $ no_info)" (mp ~level it)

let mp_alt ~mp_a ~mp_b ~level alt =
  match alt with
  | L.Left a -> Printf.sprintf "(L.Left %s)" (mp_a ~level a)
  | L.Right b -> Printf.sprintf "(L.Right %s)" (mp_b ~level b)

let mp_pair ~mp_a ~mp_b ~level (a, b) =
  Printf.sprintf "(%s, %s)" (mp_a ~level a) (mp_b ~level b)

let mp_triple ~mp_a ~mp_b ~mp_c ~level (a, b, c) =
  Printf.sprintf "(%s, %s, %s)" (mp_a ~level a) (mp_b ~level b) (mp_c ~level c)

let mp_opt ~mp ~level opt =
  match opt with
  | Some elem -> Printf.sprintf "(Some %s)" (mp ~level elem)
  | None -> "None"

let mp_list ~mp ~level l =
  let elems =
    List.map
      (fun elem ->
        Printf.sprintf "%s%s;\n"
          (indent (level + 1))
          (mp ~level:(level + 1) elem))
      l
  in
  Printf.sprintf "[\n%s%s]" (String.concat "" elems) (indent level)

type fstruct = { name : string; print : int -> string }

let fstruct ~name ~printer ~item =
  {
    name;
    print =
      (fun level ->
        Printf.sprintf "%s%s = %s;\n"
          (indent (level + 1))
          name
          (printer ~level:(level + 1) item));
  }

let mp_fstructs ~level fstructs =
  let fields = List.map (fun f -> f.print level) fstructs |> String.concat "" in
  Printf.sprintf "{\n%s%s}" fields (indent level)

type fplain = { print : int -> string }

let fplain ~printer ~item =
  {
    print =
      (fun level ->
        Printf.sprintf "%s%s"
          (indent (level + 1))
          (printer ~level:(level + 1) item));
  }

let mp_fplains ~level fplains =
  let fields =
    List.map (fun f -> f.print level) fplains |> String.concat ",\n"
  in
  Printf.sprintf "(\n%s%s)" fields (indent level)

(* Booleans *)

let mp_bool ~level bool =
  level |> ignore;
  Printf.sprintf "%b" bool

(* Numbers *)

let rec mp_num ~level num = mp_it ~mp:mp_num' ~level num.it

and mp_num' ~level num' =
  level |> ignore;
  match num' with
  | i, Some (width, signed) ->
      Printf.sprintf "(Bigint.of_int_exn %s, Some (Bigint.of_int_exn %s, %b))"
        (Bigint.to_string i) (Bigint.to_string width) signed
  | i, None ->
      Printf.sprintf "(Bigint.of_int_exn %s, None)" (Bigint.to_string i)

(* Texts *)

let rec mp_text ~level text = mp_it ~mp:mp_text' ~level text.it

and mp_text' ~level text' =
  level |> ignore;
  Printf.sprintf "\"%s\"" (String.escaped text')

and mp_texts ~level texts = mp_list ~mp:mp_text ~level texts

(* Identifiers *)

let rec mp_id ~level id = mp_it ~mp:mp_id' ~level id.it
and mp_id' ~level id' = mp_text' ~level id'

(* Variables (scoped identifiers) *)

let rec mp_var ~level var = mp_it ~mp:mp_var' ~level var.it

and mp_var' ~level var' =
  level |> ignore;
  match var' with
  | L.Top id ->
      let fplains = [ fplain ~printer:mp_id ~item:id ] in
      Printf.sprintf "L.Top %s" (mp_fplains ~level fplains)
  | L.Current id ->
      let fplains = [ fplain ~printer:mp_id ~item:id ] in
      Printf.sprintf "L.Current %s" (mp_fplains ~level fplains)

(* Members *)

let rec mp_member ~level member = mp_it ~mp:mp_member' ~level member.it
and mp_member' ~level member' = mp_text' ~level member'
and mp_members ~level members = mp_list ~mp:mp_member ~level members

(* State labels *)

let rec mp_state_label ~level state_label =
  mp_it ~mp:mp_state_label' ~level state_label.it

and mp_state_label' ~level state_label' = mp_text' ~level state_label'

(* Match kinds *)

let rec mp_match_kind ~level match_kind =
  mp_it ~mp:mp_match_kind' ~level match_kind.it

and mp_match_kind' ~level match_kind' = mp_text' ~level match_kind'

(* Unary operators *)

let rec mp_unop ~level unop = mp_it ~mp:mp_unop' ~level unop.it

and mp_unop' ~level unop' =
  level |> ignore;
  match unop' with
  | L.BNotOp -> "L.BNotOp"
  | L.LNotOp -> "L.LNotOp"
  | L.UPlusOp -> "L.UPlusOp"
  | L.UMinusOp -> "L.UMinusOp"

(* Binary operators *)

let rec mp_binop ~level binop = mp_it ~mp:mp_binop' ~level binop.it

and mp_binop' ~level binop' =
  level |> ignore;
  match binop' with
  | L.PlusOp -> "L.PlusOp"
  | L.SPlusOp -> "L.SPlusOp"
  | L.MinusOp -> "L.MinusOp"
  | L.SMinusOp -> "L.SMinusOp"
  | L.MulOp -> "L.MulOp"
  | L.DivOp -> "L.DivOp"
  | L.ModOp -> "L.ModOp"
  | L.ShlOp -> "L.ShlOp"
  | L.ShrOp -> "L.ShrOp"
  | L.LeOp -> "L.LeOp"
  | L.GeOp -> "L.GeOp"
  | L.LtOp -> "L.LtOp"
  | L.GtOp -> "L.GtOp"
  | L.EqOp -> "L.EqOp"
  | L.NeOp -> "L.NeOp"
  | L.BAndOp -> "L.BAndOp"
  | L.BXorOp -> "L.BXorOp"
  | L.BOrOp -> "L.BOrOp"
  | L.ConcatOp -> "L.ConcatOp"
  | L.LAndOp -> "L.LAndOp"
  | L.LOrOp -> "L.LOrOp"

(* Directions *)

let rec mp_dir ~level dir = mp_it ~mp:mp_dir' ~level dir.it

and mp_dir' ~level dir' =
  level |> ignore;
  match dir' with
  | L.No -> "L.No"
  | L.In -> "L.In"
  | L.Out -> "L.Out"
  | L.InOut -> "L.InOut"

(* Types *)

let rec mp_typ ~level typ = mp_it ~mp:mp_typ' ~level typ.it

and mp_typ' ~level typ' =
  match typ' with
  | VoidT -> "VoidT"
  | ErrT -> "ErrT"
  | MatchKindT -> "MatchKindT"
  | StrT -> "StrT"
  | BoolT -> "BoolT"
  | IntT -> "IntT"
  | FIntT expr ->
      let fplains = [ fplain ~printer:mp_expr ~item:expr ] in
      Printf.sprintf "FIntT %s" (mp_fplains ~level:(level + 1) fplains)
  | FBitT expr ->
      let fplains = [ fplain ~printer:mp_expr ~item:expr ] in
      Printf.sprintf "FBitT %s" (mp_fplains ~level:(level + 1) fplains)
  | VBitT expr ->
      let fplains = [ fplain ~printer:mp_expr ~item:expr ] in
      Printf.sprintf "VBitT %s" (mp_fplains ~level:(level + 1) fplains)
  | NameT var ->
      let fplains = [ fplain ~printer:mp_var ~item:var ] in
      Printf.sprintf "NameT %s" (mp_fplains ~level:(level + 1) fplains)
  | SpecT (var, targs) ->
      let fplains =
        [
          fplain ~printer:mp_var ~item:var; fplain ~printer:mp_targs ~item:targs;
        ]
      in
      Printf.sprintf "SpecT %s" (mp_fplains ~level:(level + 1) fplains)
  | StackT (typ, expr) ->
      let fplains =
        [ fplain ~printer:mp_typ ~item:typ; fplain ~printer:mp_expr ~item:expr ]
      in
      Printf.sprintf "StackT %s" (mp_fplains ~level:(level + 1) fplains)
  | ListT typ ->
      let fplains = [ fplain ~printer:mp_typ ~item:typ ] in
      Printf.sprintf "ListT %s" (mp_fplains ~level:(level + 1) fplains)
  | TupleT typs ->
      let fplains = [ fplain ~printer:mp_typs ~item:typs ] in
      Printf.sprintf "TupleT %s" (mp_fplains ~level:(level + 1) fplains)
  | AnyT -> "AnyT"

and mp_typs ~level typs = mp_list ~mp:mp_typ ~level typs

(* Annotations *)

and mp_anno ~level anno = mp_it ~mp:mp_anno' ~level anno.it

and mp_anno' ~level anno' =
  match anno' with
  | L.EmptyN text ->
      let fplains = [ fplain ~printer:mp_text ~item:text ] in
      Printf.sprintf "L.EmptyN %s" (mp_fplains ~level:(level + 1) fplains)
  | L.TextN (text, texts) ->
      let fplains =
        [
          fplain ~printer:mp_text ~item:text;
          fplain ~printer:mp_texts ~item:texts;
        ]
      in
      Printf.sprintf "L.TextN %s" (mp_fplains ~level:(level + 1) fplains)
  | L.ExprN (text, exprs) ->
      let fplains =
        [
          fplain ~printer:mp_text ~item:text;
          fplain ~printer:mp_exprs ~item:exprs;
        ]
      in
      Printf.sprintf "L.ExprN %s" (mp_fplains ~level:(level + 1) fplains)
  | L.RecordN (text, fields) ->
      let fplains =
        [
          fplain ~printer:mp_text ~item:text;
          fplain
            ~printer:(mp_list ~mp:(mp_pair ~mp_a:mp_text ~mp_b:mp_expr))
            ~item:fields;
        ]
      in
      Printf.sprintf "L.RecordN %s" (mp_fplains ~level:(level + 1) fplains)

and mp_annos ~level annos = mp_list ~mp:mp_anno ~level annos

(* Type parameters *)

and mp_tparam ~level tparam = mp_it ~mp:mp_tparam' ~level tparam.it
and mp_tparam' ~level tparam' = mp_id' ~level tparam'
and mp_tparams ~level tparams = mp_list ~mp:mp_tparam ~level tparams

(* Parameters *)

and mp_param ~level param = mp_it ~mp:mp_param' ~level param.it

and mp_param' ~level param' =
  let id, dir, typ, expr_opt, annos = param' in
  Printf.sprintf "(%s, %s, %s, %s, %s)" (mp_id ~level id) (mp_dir ~level dir)
    (mp_typ ~level typ)
    (mp_opt ~mp:mp_expr ~level expr_opt)
    (mp_annos ~level annos)

and mp_params ~level params = mp_list ~mp:mp_param ~level params

(* Constructor parameters *)

and mp_cparam ~level cparam = mp_it ~mp:mp_cparam' ~level cparam.it
and mp_cparam' ~level cparam' = mp_param' ~level cparam'
and mp_cparams ~level cparams = mp_list ~mp:mp_cparam ~level cparams

(* Type arguments *)

and mp_targ ~level targ = mp_it ~mp:mp_targ' ~level targ.it
and mp_targ' ~level targ' = mp_typ' ~level targ'
and mp_targs ~level targs = mp_list ~mp:mp_targ ~level targs

(* Arguments *)

and mp_arg ~level arg = mp_it ~mp:mp_arg' ~level arg.it

and mp_arg' ~level arg' =
  match arg' with
  | L.ExprA expr ->
      let fplains = [ fplain ~printer:mp_expr ~item:expr ] in
      Printf.sprintf "L.ExprA %s" (mp_fplains ~level:(level + 1) fplains)
  | L.NameA (id, expr_opt) ->
      let fplains =
        [
          fplain ~printer:mp_id ~item:id;
          fplain ~printer:(mp_opt ~mp:mp_expr) ~item:expr_opt;
        ]
      in
      Printf.sprintf "L.NameA %s" (mp_fplains ~level:(level + 1) fplains)
  | L.AnyA -> "L.AnyA"

and mp_args ~level args = mp_list ~mp:mp_arg ~level args

(* Expressions *)

and mp_expr ~level expr = mp_it ~mp:mp_expr' ~level expr.it

and mp_expr' ~level expr' =
  match expr' with
  | BoolE { boolean } ->
      let fstructs =
        [ fstruct ~name:"boolean" ~printer:mp_bool ~item:boolean ]
      in
      Printf.sprintf "BoolE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | StrE { text } ->
      let fstructs = [ fstruct ~name:"text" ~printer:mp_text ~item:text ] in
      Printf.sprintf "StrE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | NumE { num } ->
      let fstructs = [ fstruct ~name:"num" ~printer:mp_num ~item:num ] in
      Printf.sprintf "NumE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | VarE { var } ->
      let fstructs = [ fstruct ~name:"var" ~printer:mp_var ~item:var ] in
      Printf.sprintf "VarE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | SeqE { exprs } ->
      let fstructs = [ fstruct ~name:"exprs" ~printer:mp_exprs ~item:exprs ] in
      Printf.sprintf "SeqE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | SeqDefaultE { exprs } ->
      let fstructs = [ fstruct ~name:"exprs" ~printer:mp_exprs ~item:exprs ] in
      Printf.sprintf "SeqDefaultE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | RecordE { fields } ->
      let fstructs =
        [
          fstruct ~name:"fields"
            ~printer:(mp_list ~mp:(mp_pair ~mp_a:mp_text ~mp_b:mp_expr))
            ~item:fields;
        ]
      in
      Printf.sprintf "RecordE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | RecordDefaultE { fields } ->
      let fstructs =
        [
          fstruct ~name:"fields"
            ~printer:(mp_list ~mp:(mp_pair ~mp_a:mp_text ~mp_b:mp_expr))
            ~item:fields;
        ]
      in
      Printf.sprintf "RecordDefaultE %s"
        (mp_fstructs ~level:(level + 1) fstructs)
  | DefaultE -> "DefaultE"
  | InvalidE -> "InvalidE"
  | UnE { unop; expr } ->
      let fstructs =
        [
          fstruct ~name:"unop" ~printer:mp_unop ~item:unop;
          fstruct ~name:"expr" ~printer:mp_expr ~item:expr;
        ]
      in
      Printf.sprintf "UnE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | BinE { binop; expr_l; expr_r } ->
      let fstructs =
        [
          fstruct ~name:"binop" ~printer:mp_binop ~item:binop;
          fstruct ~name:"expr_l" ~printer:mp_expr ~item:expr_l;
          fstruct ~name:"expr_r" ~printer:mp_expr ~item:expr_r;
        ]
      in
      Printf.sprintf "BinE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | TernE { expr_cond; expr_then; expr_else } ->
      let fstructs =
        [
          fstruct ~name:"expr_cond" ~printer:mp_expr ~item:expr_cond;
          fstruct ~name:"expr_then" ~printer:mp_expr ~item:expr_then;
          fstruct ~name:"expr_else" ~printer:mp_expr ~item:expr_else;
        ]
      in
      Printf.sprintf "TernE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | CastE { typ; expr } ->
      let fstructs =
        [
          fstruct ~name:"typ" ~printer:mp_typ ~item:typ;
          fstruct ~name:"expr" ~printer:mp_expr ~item:expr;
        ]
      in
      Printf.sprintf "CastE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | MaskE { expr_base; expr_mask } ->
      let fstructs =
        [
          fstruct ~name:"expr_base" ~printer:mp_expr ~item:expr_base;
          fstruct ~name:"expr_mask" ~printer:mp_expr ~item:expr_mask;
        ]
      in
      Printf.sprintf "MaskE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | RangeE { expr_lb; expr_ub } ->
      let fstructs =
        [
          fstruct ~name:"expr_lb" ~printer:mp_expr ~item:expr_lb;
          fstruct ~name:"expr_ub" ~printer:mp_expr ~item:expr_ub;
        ]
      in
      Printf.sprintf "RangeE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | SelectE { exprs_select; cases } ->
      let fstructs =
        [
          fstruct ~name:"exprs_select" ~printer:mp_exprs ~item:exprs_select;
          fstruct ~name:"cases" ~printer:mp_select_cases ~item:cases;
        ]
      in
      Printf.sprintf "SelectE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ArrAccE { expr_base; expr_idx } ->
      let fstructs =
        [
          fstruct ~name:"expr_base" ~printer:mp_expr ~item:expr_base;
          fstruct ~name:"expr_idx" ~printer:mp_expr ~item:expr_idx;
        ]
      in
      Printf.sprintf "ArrAccE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | BitAccE { expr_base; expr_lo; expr_hi } ->
      let fstructs =
        [
          fstruct ~name:"expr_base" ~printer:mp_expr ~item:expr_base;
          fstruct ~name:"expr_lo" ~printer:mp_expr ~item:expr_lo;
          fstruct ~name:"expr_hi" ~printer:mp_expr ~item:expr_hi;
        ]
      in
      Printf.sprintf "BitAccE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ErrAccE { member } ->
      let fstructs =
        [ fstruct ~name:"member" ~printer:mp_member ~item:member ]
      in
      Printf.sprintf "ErrAccE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | TypeAccE { var_base; member } ->
      let fstructs =
        [
          fstruct ~name:"var_base" ~printer:mp_var ~item:var_base;
          fstruct ~name:"member" ~printer:mp_member ~item:member;
        ]
      in
      Printf.sprintf "TypeAccE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ExprAccE { expr_base; member } ->
      let fstructs =
        [
          fstruct ~name:"expr_base" ~printer:mp_expr ~item:expr_base;
          fstruct ~name:"member" ~printer:mp_member ~item:member;
        ]
      in
      Printf.sprintf "ExprAccE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | CallFuncE { var_func; targs; args } ->
      let fstructs =
        [
          fstruct ~name:"var_func" ~printer:mp_var ~item:var_func;
          fstruct ~name:"targs" ~printer:mp_targs ~item:targs;
          fstruct ~name:"args" ~printer:mp_args ~item:args;
        ]
      in
      Printf.sprintf "CallFuncE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | CallMethodE { expr_base; member; targs; args } ->
      let fstructs =
        [
          fstruct ~name:"expr_base" ~printer:mp_expr ~item:expr_base;
          fstruct ~name:"member" ~printer:mp_member ~item:member;
          fstruct ~name:"targs" ~printer:mp_targs ~item:targs;
          fstruct ~name:"args" ~printer:mp_args ~item:args;
        ]
      in
      Printf.sprintf "CallMethodE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | CallTypeE { var_typ; member; targs; args } ->
      let fstructs =
        [
          fstruct ~name:"var_typ" ~printer:mp_var ~item:var_typ;
          fstruct ~name:"member" ~printer:mp_member ~item:member;
          fstruct ~name:"targs" ~printer:mp_targs ~item:targs;
          fstruct ~name:"args" ~printer:mp_args ~item:args;
        ]
      in
      Printf.sprintf "CallTypeE %s" (mp_fstructs ~level:(level + 1) fstructs)
  | InstE { var_inst; targs; args } ->
      let fstructs =
        [
          fstruct ~name:"var_inst" ~printer:mp_var ~item:var_inst;
          fstruct ~name:"targs" ~printer:mp_targs ~item:targs;
          fstruct ~name:"args" ~printer:mp_args ~item:args;
        ]
      in
      Printf.sprintf "InstE %s" (mp_fstructs ~level:(level + 1) fstructs)

and mp_exprs ~level exprs = mp_list ~mp:mp_expr ~level exprs

(* Keyset expressions *)

and mp_keyset ~level keyset = mp_it ~mp:mp_keyset' ~level keyset.it

and mp_keyset' ~level keyset' =
  match keyset' with
  | L.ExprK expr ->
      let fplains = [ fplain ~printer:mp_expr ~item:expr ] in
      Printf.sprintf "L.ExprK %s" (mp_fplains ~level:(level + 1) fplains)
  | L.DefaultK -> "L.DefaultK"
  | L.AnyK -> "L.AnyK"

and mp_keysets ~level keysets = mp_list ~mp:mp_keyset ~level keysets

(* Select-cases for select *)

and mp_select_case ~level select_case =
  mp_it ~mp:mp_select_case' ~level select_case.it

and mp_select_case' ~level select_case' =
  let keysets, state_label = select_case' in
  Printf.sprintf "(%s, %s)"
    (mp_keysets ~level keysets)
    (mp_state_label ~level state_label)

and mp_select_cases ~level select_cases =
  mp_list ~mp:mp_select_case ~level select_cases

(* Statements *)

and mp_stmt ~level stmt = mp_it ~mp:mp_stmt' ~level stmt.it

and mp_stmt' ~level stmt' =
  match stmt' with
  | EmptyS -> "EmptyS"
  | AssignS { expr_l; expr_r } ->
      let fstructs =
        [
          fstruct ~name:"expr_l" ~printer:mp_expr ~item:expr_l;
          fstruct ~name:"expr_r" ~printer:mp_expr ~item:expr_r;
        ]
      in
      Printf.sprintf "AssignS %s" (mp_fstructs ~level:(level + 1) fstructs)
  | SwitchS { expr_switch; cases } ->
      let fstructs =
        [
          fstruct ~name:"expr_switch" ~printer:mp_expr ~item:expr_switch;
          fstruct ~name:"cases" ~printer:mp_switch_cases ~item:cases;
        ]
      in
      Printf.sprintf "SwitchS %s" (mp_fstructs ~level:(level + 1) fstructs)
  | IfS { expr_cond; stmt_then; stmt_else } ->
      let fstructs =
        [
          fstruct ~name:"expr_cond" ~printer:mp_expr ~item:expr_cond;
          fstruct ~name:"stmt_then" ~printer:mp_stmt ~item:stmt_then;
          fstruct ~name:"stmt_else" ~printer:mp_stmt ~item:stmt_else;
        ]
      in
      Printf.sprintf "IfS %s" (mp_fstructs ~level:(level + 1) fstructs)
  | BlockS { block } ->
      let fstructs = [ fstruct ~name:"block" ~printer:mp_block ~item:block ] in
      Printf.sprintf "BlockS %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ExitS -> "ExitS"
  | RetS { expr_ret } ->
      let fplains = [ fplain ~printer:(mp_opt ~mp:mp_expr) ~item:expr_ret ] in
      Printf.sprintf "RetS %s" (mp_fplains ~level:(level + 1) fplains)
  | CallFuncS { var_func; targs; args } ->
      let fstructs =
        [
          fstruct ~name:"var_func" ~printer:mp_var ~item:var_func;
          fstruct ~name:"targs" ~printer:mp_targs ~item:targs;
          fstruct ~name:"args" ~printer:mp_args ~item:args;
        ]
      in
      Printf.sprintf "CallFuncS %s" (mp_fstructs ~level:(level + 1) fstructs)
  | CallMethodS { expr_base; member; targs; args } ->
      let fstructs =
        [
          fstruct ~name:"expr_base" ~printer:mp_expr ~item:expr_base;
          fstruct ~name:"member" ~printer:mp_member ~item:member;
          fstruct ~name:"targs" ~printer:mp_targs ~item:targs;
          fstruct ~name:"args" ~printer:mp_args ~item:args;
        ]
      in
      Printf.sprintf "CallMethodS %s" (mp_fstructs ~level:(level + 1) fstructs)
  | CallInstS { var_inst; targs; args } ->
      let fstructs =
        [
          fstruct ~name:"var_inst" ~printer:mp_var ~item:var_inst;
          fstruct ~name:"targs" ~printer:mp_targs ~item:targs;
          fstruct ~name:"args" ~printer:mp_args ~item:args;
        ]
      in
      Printf.sprintf "CallInstS %s" (mp_fstructs ~level:(level + 1) fstructs)
  | TransS { expr_label } ->
      let fstructs =
        [ fstruct ~name:"expr_label" ~printer:mp_expr ~item:expr_label ]
      in
      Printf.sprintf "TransS %s" (mp_fstructs ~level:(level + 1) fstructs)
  | DeclS { decl } ->
      let fstructs = [ fstruct ~name:"decl" ~printer:mp_decl ~item:decl ] in
      Printf.sprintf "DeclS %s" (mp_fstructs ~level:(level + 1) fstructs)

and mp_stmts ~level stmts = mp_list ~mp:mp_stmt ~level stmts

(* Blocks *)

and mp_block ~level block = mp_it ~mp:mp_block' ~level block.it

and mp_block' ~level block' =
  let stmts, annos = block' in
  Printf.sprintf "(%s, %s)" (mp_stmts ~level stmts) (mp_annos ~level annos)

(* Match-cases for switch *)

and mp_switch_label ~level switch_label =
  mp_it ~mp:mp_switch_label' ~level switch_label.it

and mp_switch_label' ~level switch_label' =
  match switch_label' with
  | L.ExprL expr ->
      let fplains = [ fplain ~printer:mp_expr ~item:expr ] in
      Printf.sprintf "L.ExprL %s" (mp_fplains ~level:(level + 1) fplains)
  | L.DefaultL -> "L.DefaultL"

and mp_switch_case ~level switch_case =
  mp_it ~mp:mp_switch_case' ~level switch_case.it

and mp_switch_case' ~level switch_case' =
  match switch_case' with
  | L.MatchC (switch_label, block) ->
      let fplains =
        [
          fplain ~printer:mp_switch_label ~item:switch_label;
          fplain ~printer:mp_block ~item:block;
        ]
      in
      Printf.sprintf "L.MatchC %s" (mp_fplains ~level:(level + 1) fplains)
  | L.FallC switch_label ->
      let fplains = [ fplain ~printer:mp_switch_label ~item:switch_label ] in
      Printf.sprintf "L.FallC %s" (mp_fplains ~level:(level + 1) fplains)

and mp_switch_cases ~level switch_cases =
  mp_list ~mp:mp_switch_case ~level switch_cases

(* Declarations *)

and mp_decl ~level decl = mp_it ~mp:mp_decl' ~level decl.it

and mp_decl' ~level decl' =
  match decl' with
  | ConstD { id; typ; value; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"typ" ~printer:mp_typ ~item:typ;
          fstruct ~name:"value" ~printer:mp_expr ~item:value;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "ConstD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | VarD { id; typ; init; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"typ" ~printer:mp_typ ~item:typ;
          fstruct ~name:"init" ~printer:(mp_opt ~mp:mp_expr) ~item:init;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "VarD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ErrD { members } ->
      let fstructs =
        [ fstruct ~name:"members" ~printer:mp_members ~item:members ]
      in
      Printf.sprintf "ErrD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | MatchKindD { members } ->
      let fstructs =
        [ fstruct ~name:"members" ~printer:mp_members ~item:members ]
      in
      Printf.sprintf "MatchKindD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | InstD { id; var_inst; targs; args; init; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"var_inst" ~printer:mp_var ~item:var_inst;
          fstruct ~name:"targs" ~printer:mp_targs ~item:targs;
          fstruct ~name:"args" ~printer:mp_args ~item:args;
          fstruct ~name:"init" ~printer:(mp_list ~mp:mp_decl) ~item:init;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "InstD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | StructD { id; tparams; fields; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"fields"
            ~printer:
              (mp_list
                 ~mp:(mp_triple ~mp_a:mp_text ~mp_b:mp_typ ~mp_c:mp_annos))
            ~item:fields;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "StructD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | HeaderD { id; tparams; fields; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"fields"
            ~printer:
              (mp_list
                 ~mp:(mp_triple ~mp_a:mp_text ~mp_b:mp_typ ~mp_c:mp_annos))
            ~item:fields;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "HeaderD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | UnionD { id; tparams; fields; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"fields"
            ~printer:
              (mp_list
                 ~mp:(mp_triple ~mp_a:mp_text ~mp_b:mp_typ ~mp_c:mp_annos))
            ~item:fields;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "UnionD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | EnumD { id; members; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"members" ~printer:mp_members ~item:members;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "EnumD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | SEnumD { id; typ; fields; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"typ" ~printer:mp_typ ~item:typ;
          fstruct ~name:"fields"
            ~printer:(mp_list ~mp:(mp_pair ~mp_a:mp_member ~mp_b:mp_expr))
            ~item:fields;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "SEnumD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | NewTypeD { id; typdef; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"typdef"
            ~printer:(mp_alt ~mp_a:mp_typ ~mp_b:mp_decl)
            ~item:typdef;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "NewTypeD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | TypeDefD { id; typdef; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"typdef"
            ~printer:(mp_alt ~mp_a:mp_typ ~mp_b:mp_decl)
            ~item:typdef;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "TypeDefD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ValueSetD { id; typ; size; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"typ" ~printer:mp_typ ~item:typ;
          fstruct ~name:"size" ~printer:mp_expr ~item:size;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "ValueSetD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ParserTypeD { id; tparams; params; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"params" ~printer:mp_params ~item:params;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "ParserTypeD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ParserD { id; tparams; params; cparams; locals; states; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"params" ~printer:mp_params ~item:params;
          fstruct ~name:"cparams" ~printer:mp_cparams ~item:cparams;
          fstruct ~name:"locals" ~printer:mp_decls ~item:locals;
          fstruct ~name:"states" ~printer:mp_parser_states ~item:states;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "ParserD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | TableD { id; table; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"table" ~printer:mp_table ~item:table;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "TableD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ControlTypeD { id; tparams; params; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"params" ~printer:mp_params ~item:params;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "ControlTypeD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ControlD { id; tparams; params; cparams; locals; body; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"params" ~printer:mp_params ~item:params;
          fstruct ~name:"cparams" ~printer:mp_cparams ~item:cparams;
          fstruct ~name:"locals" ~printer:mp_decls ~item:locals;
          fstruct ~name:"body" ~printer:mp_block ~item:body;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "ControlD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ActionD { id; params; body; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"params" ~printer:mp_params ~item:params;
          fstruct ~name:"body" ~printer:mp_block ~item:body;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "ActionD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | FuncD { id; typ_ret; tparams; params; body } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"typ_ret" ~printer:mp_typ ~item:typ_ret;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"params" ~printer:mp_params ~item:params;
          fstruct ~name:"body" ~printer:mp_block ~item:body;
        ]
      in
      Printf.sprintf "FuncD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ExternFuncD { id; typ_ret; tparams; params; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"typ_ret" ~printer:mp_typ ~item:typ_ret;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"params" ~printer:mp_params ~item:params;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "ExternFuncD %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ExternObjectD { id; tparams; mthds; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"mthds" ~printer:mp_mthds ~item:mthds;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "ExternObjectD %s"
        (mp_fstructs ~level:(level + 1) fstructs)
  | PackageTypeD { id; tparams; cparams; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"cparams" ~printer:mp_cparams ~item:cparams;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "PackageTypeD %s" (mp_fstructs ~level:(level + 1) fstructs)

and mp_decls ~level decls = mp_list ~mp:mp_decl ~level decls

(* Parser state machine *)

and mp_parser_state ~level parser_state =
  mp_it ~mp:mp_parser_state' ~level parser_state.it

and mp_parser_state' ~level parser_state' =
  let state_label, block, annos = parser_state' in
  Printf.sprintf "(%s, %s, %s)"
    (mp_state_label ~level state_label)
    (mp_block ~level block) (mp_annos ~level annos)

and mp_parser_states ~level parser_states =
  mp_list ~mp:mp_parser_state ~level parser_states

(* Tables *)

and mp_table ~level table = mp_table_properties ~level table

(* Table properties *)

and mp_table_property ~level table_property =
  match table_property with
  | L.KeyP table_keys ->
      let fplains = [ fplain ~printer:mp_table_keys ~item:table_keys ] in
      Printf.sprintf "L.KeyP %s" (mp_fplains ~level:(level + 1) fplains)
  | L.ActionP table_actions ->
      let fplains = [ fplain ~printer:mp_table_actions ~item:table_actions ] in
      Printf.sprintf "L.ActionP %s" (mp_fplains ~level:(level + 1) fplains)
  | L.EntryP table_entries ->
      let fplains = [ fplain ~printer:mp_table_entries ~item:table_entries ] in
      Printf.sprintf "L.EntryP %s" (mp_fplains ~level:(level + 1) fplains)
  | L.DefaultP table_default ->
      let fplains = [ fplain ~printer:mp_table_default ~item:table_default ] in
      Printf.sprintf "L.DefaultP %s" (mp_fplains ~level:(level + 1) fplains)
  | L.CustomP table_custom ->
      let fplains = [ fplain ~printer:mp_table_custom ~item:table_custom ] in
      Printf.sprintf "L.CustomP %s" (mp_fplains ~level:(level + 1) fplains)

and mp_table_properties ~level table_properties =
  mp_list ~mp:mp_table_property ~level table_properties

(* Table keys *)

and mp_table_key ~level table_key = mp_it ~mp:mp_table_key' ~level table_key.it

and mp_table_key' ~level table_key' =
  let expr, match_kind, annos = table_key' in
  Printf.sprintf "(%s, %s, %s)" (mp_expr ~level expr)
    (mp_match_kind ~level match_kind)
    (mp_annos ~level annos)

and mp_table_keys ~level table_keys =
  mp_it ~mp:mp_table_keys' ~level table_keys.it

and mp_table_keys' ~level table_keys' =
  mp_list ~mp:mp_table_key ~level table_keys'

(* Table action references *)

and mp_table_action ~level table_action =
  mp_it ~mp:mp_table_action' ~level table_action.it

and mp_table_action' ~level table_action' =
  let var, args, annos = table_action' in
  Printf.sprintf "(%s, %s, %s)" (mp_var ~level var) (mp_args ~level args)
    (mp_annos ~level annos)

and mp_table_actions ~level table_actions =
  mp_it ~mp:mp_table_actions' ~level table_actions.it

and mp_table_actions' ~level table_actions' =
  mp_list ~mp:mp_table_action ~level table_actions'

(* Table entries *)

and mp_table_entry ~level table_entry =
  mp_it ~mp:mp_table_entry' ~level table_entry.it

and mp_table_entry' ~level table_entry' =
  let table_entry_const, keysets, table_action, expr_opt, annos =
    table_entry'
  in
  Printf.sprintf "(%s, %s, %s, %s, %s)"
    (mp_bool ~level table_entry_const)
    (mp_keysets ~level keysets)
    (mp_table_action ~level table_action)
    (mp_opt ~mp:mp_expr ~level expr_opt)
    (mp_annos ~level annos)

and mp_table_entries ~level table_entries =
  mp_it ~mp:mp_table_entries' ~level table_entries.it

and mp_table_entries' ~level table_entries' =
  let table_entries_const, entries = table_entries' in
  Printf.sprintf "(%s, %s)"
    (mp_bool ~level table_entries_const)
    (mp_list ~mp:mp_table_entry ~level entries)

(* Table default properties *)

and mp_table_default ~level table_default =
  mp_it ~mp:mp_table_default' ~level table_default.it

and mp_table_default' ~level table_default' =
  let table_default_const, table_action = table_default' in
  Printf.sprintf "(%s, %s)"
    (mp_bool ~level table_default_const)
    (mp_table_action ~level table_action)

(* Table custom properties *)

and mp_table_custom ~level table_custom =
  mp_it ~mp:mp_table_custom' ~level table_custom.it

and mp_table_custom' ~level table_custom' =
  let table_custom_const, member, expr, annos = table_custom' in
  Printf.sprintf "(%s, %s, %s, %s)"
    (mp_bool ~level table_custom_const)
    (mp_member ~level member) (mp_expr ~level expr) (mp_annos ~level annos)

(* Methods *)

and mp_mthd ~level mthd = mp_it ~mp:mp_mthd' ~level mthd.it

and mp_mthd' ~level mthd' =
  match mthd' with
  | ExternConsM { id; cparams; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"cparams" ~printer:mp_cparams ~item:cparams;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "ExternConsM %s" (mp_fstructs ~level:(level + 1) fstructs)
  | ExternAbstractM { id; typ_ret; tparams; params; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"typ_ret" ~printer:mp_typ ~item:typ_ret;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"params" ~printer:mp_params ~item:params;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "ExternAbstractM %s"
        (mp_fstructs ~level:(level + 1) fstructs)
  | ExternM { id; typ_ret; tparams; params; annos } ->
      let fstructs =
        [
          fstruct ~name:"id" ~printer:mp_id ~item:id;
          fstruct ~name:"typ_ret" ~printer:mp_typ ~item:typ_ret;
          fstruct ~name:"tparams" ~printer:mp_tparams ~item:tparams;
          fstruct ~name:"params" ~printer:mp_params ~item:params;
          fstruct ~name:"annos" ~printer:mp_annos ~item:annos;
        ]
      in
      Printf.sprintf "ExternM %s" (mp_fstructs ~level:(level + 1) fstructs)

and mp_mthds ~level mthds = mp_list ~mp:mp_mthd ~level mthds

(* Program *)

let mp_program program = "let program = " ^ mp_list ~mp:mp_decl ~level:1 program
