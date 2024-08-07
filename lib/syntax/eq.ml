open Util.Source
open Ast
open Pp

(* Utils *)

let check (category : string) (eq : 'a -> 'a -> bool) pp (a : 'a) (b : 'a) =
  let same = eq a b in
  if not same then
    Format.eprintf "eq_%s: %a does not equal %a\n" category pp a pp b;
  same

let eq_alt (eq_a : 'a -> 'a -> bool) (eq_b : 'b -> 'b -> bool)
    (alt : ('a, 'b) alt) (alt' : ('a, 'b) alt) =
  match (alt, alt') with
  | Left x, Left y -> eq_a x y
  | Right x, Right y -> eq_b x y
  | _ -> false

let eq_option (eq : 'a -> 'a -> bool) (opt : 'a option) (opt' : 'a option) =
  match (opt, opt') with
  | None, None -> true
  | Some x, Some y -> eq x y
  | _ -> false

let eq_list (eq : 'a -> 'a -> bool) (l : 'a list) (l' : 'a list) =
  List.length l = List.length l' && List.for_all2 eq l l'

(* Numbers *)

let rec eq_num (num : num) (num' : num) = check "num" eq_num' pp_num num num'
and eq_num' (num : num) (num' : num) = num.it = num'.it

(* Names *)

let rec eq_id (id : id) (id' : id) = check "id" eq_id' pp_id id id'
and eq_id' (id : id) (id' : id) = id.it = id'.it

let rec eq_path (path : path) (path' : path) =
  check "path" eq_path' pp_path path path'

and eq_path' (path : path) (path' : path) =
  let path = List.map it path in
  let path' = List.map it path' in
  path = path'

let rec eq_var (var : var) (var' : var) = check "var" eq_var' pp_var var var'

and eq_var' (var : var) (var' : var) =
  match (var.it, var'.it) with
  | Top id, Top id' -> eq_id id id'
  | Bare id, Bare id' -> eq_id id id'
  | _ -> false

let rec eq_member (member : member) (member' : member) =
  check "member" eq_member' pp_member member member'

and eq_member' (member : member) (member' : member) = member.it = member'.it

let rec eq_label (label : label) (label' : label) =
  check "label" eq_label' pp_label label label'

and eq_label' (label : label) (label' : label) = label.it = label'.it

let rec eq_mtch_kind (mtch_kind : mtch_kind) (mtch_kind' : mtch_kind) =
  check "mtch_kind" eq_mtch_kind' pp_mtch_kind mtch_kind mtch_kind'

and eq_mtch_kind' (mtch_kind : mtch_kind) (mtch_kind' : mtch_kind) =
  mtch_kind.it = mtch_kind'.it

(* Unary and binary operators *)

let rec eq_unop (unop : unop) (unop' : unop) =
  check "unop" eq_unop' pp_unop unop unop'

and eq_unop' (unop : unop) (unop' : unop) = unop.it = unop'.it

let rec eq_binop (binop : binop) (binop' : binop) =
  check "binop" eq_binop' pp_binop binop binop'

and eq_binop' (binop : binop) (binop' : binop) = binop.it = binop'.it

(* Types *)

let rec eq_type (typ : typ) (typ' : typ) = check "typ" eq_type' pp_type typ typ'

and eq_type' (typ : typ) (typ' : typ) =
  match (typ.it, typ'.it) with
  | VoidT, VoidT -> true
  | BoolT, BoolT -> true
  | ErrT, ErrT -> true
  | StrT, StrT -> true
  | AIntT, AIntT -> true
  | IntT expr, IntT expr' -> eq_expr expr expr'
  | BitT expr, BitT expr' -> eq_expr expr expr'
  | VBitT expr, VBitT expr' -> eq_expr expr expr'
  | NameT var, NameT var' -> eq_var var var'
  | SpecT (var, typs), SpecT (var', typs') ->
      eq_var var var' && eq_list eq_type typs typs'
  | StackT (typ, expr), StackT (typ', expr') ->
      eq_type typ typ' && eq_expr expr expr'
  | TupleT typs, TupleT typs' -> eq_list eq_type typs typs'
  | AnyT, AnyT -> true
  | _ -> false

(* Directions *)

and eq_dir (dir : dir) (dir' : dir) = check "dir" eq_dir' pp_dir dir dir'
and eq_dir' (dir : dir) (dir' : dir) = dir.it = dir'.it

(* Parameters *)

and eq_tparam (tparam : tparam) (tparam' : tparam) =
  check "tparam" eq_tparam' pp_tparam tparam tparam'

and eq_tparam' (tparam : tparam) (tparam' : tparam) = tparam.it = tparam'.it

and eq_param (param : param) (param' : param) =
  check "param" eq_param' pp_param param param'

and eq_param' (param : param) (param' : param) =
  let id, dir, typ, expr = param.it in
  let id', dir', typ', expr' = param'.it in
  eq_id id id' && eq_dir dir dir' && eq_type typ typ'
  && eq_option eq_expr expr expr'

and eq_cparam (cparam : cparam) (cparam' : cparam) =
  check "cparam" eq_param' pp_cparam cparam cparam'

(* Arguments *)

and eq_arg (arg : arg) (arg' : arg) = check "arg" eq_arg' pp_arg arg arg'

and eq_arg' (arg : arg) (arg' : arg) =
  match (arg.it, arg'.it) with
  | ExprA expr, ExprA expr' -> eq_expr expr expr'
  | NameA (id, expr), NameA (id', expr') -> eq_id id id' && eq_expr expr expr'
  | AnyA, AnyA -> true
  | _ -> false

(* Expressions *)

and eq_expr (expr : expr) (expr' : expr) =
  check "expr" eq_expr' pp_expr expr expr'

and eq_expr' (expr : expr) (expr' : expr) =
  match (expr.it, expr'.it) with
  | BoolE b, BoolE b' -> b = b'
  | StrE s, StrE s' -> s = s'
  | NumE n, NumE n' -> eq_num n n'
  | VarE var, VarE var' -> eq_var var var'
  | ListE exprs, ListE exprs' -> eq_list eq_expr exprs exprs'
  | RecordE fields, RecordE fields' ->
      eq_list
        (fun (member, expr) (member', expr') ->
          eq_member member member' && eq_expr expr expr')
        fields fields'
  | UnE (unop, expr), UnE (unop', expr') ->
      eq_unop unop unop' && eq_expr expr expr'
  | BinE (binop, expr_l, expr_r), BinE (binop', expr_l', expr_r') ->
      eq_binop binop binop' && eq_expr expr_l expr_l' && eq_expr expr_r expr_r'
  | ( TernE (expr_cond, expr_then, expr_else),
      TernE (expr_cond', expr_then', expr_else') ) ->
      eq_expr expr_cond expr_cond'
      && eq_expr expr_then expr_then'
      && eq_expr expr_else expr_else'
  | CastE (typ, expr), CastE (typ', expr') ->
      eq_type typ typ' && eq_expr expr expr'
  | MaskE (expr_l, expr_r), MaskE (expr_l', expr_r') ->
      eq_expr expr_l expr_l' && eq_expr expr_r expr_r'
  | RangeE (expr_l, expr_r), RangeE (expr_l', expr_r') ->
      eq_expr expr_l expr_l' && eq_expr expr_r expr_r'
  | ArrAccE (expr_b, expr_i), ArrAccE (expr_b', expr_i') ->
      eq_expr expr_b expr_b' && eq_expr expr_i expr_i'
  | BitAccE (expr_b, expr_l, expr_h), BitAccE (expr_b', expr_l', expr_h') ->
      eq_expr expr_b expr_b' && eq_expr expr_l expr_l' && eq_expr expr_h expr_h'
  | TypeAccE (var, member), TypeAccE (var', member') ->
      eq_var var var' && eq_member member member'
  | ErrAccE member, ErrAccE member' -> eq_member member member'
  | ExprAccE (expr, member), ExprAccE (expr', member') ->
      eq_expr expr expr' && eq_member member member'
  | CallE (expr, typs, args), CallE (expr', typs', args') ->
      eq_expr expr expr' && eq_list eq_type typs typs'
      && eq_list eq_arg args args'
  | InstE (typ, args), InstE (typ', args') ->
      eq_type typ typ' && eq_list eq_arg args args'
  | _ -> false

(* Statements *)

and eq_stmt (stmt : stmt) (stmt' : stmt) =
  check "stmt" eq_stmt' pp_stmt (0, stmt) (0, stmt')

and eq_stmt' (_, stmt) (_, stmt') =
  match (stmt.it, stmt'.it) with
  | EmptyI, EmptyI -> true
  | AssignI (expr_l, expr_r), AssignI (expr_l', expr_r') ->
      eq_expr expr_l expr_l' && eq_expr expr_r expr_r'
  | SwitchI (expr, cases), SwitchI (expr', cases') ->
      eq_expr expr expr' && eq_list eq_switch_case cases cases'
  | IfI (expr, stmt_then, stmt_else), IfI (expr', stmt_then', stmt_else') ->
      eq_expr expr expr'
      && eq_stmt stmt_then stmt_then'
      && eq_stmt stmt_else stmt_else'
  | BlockI block, BlockI block' -> eq_block block block'
  | ExitI, ExitI -> true
  | RetI expr, RetI expr' -> eq_option eq_expr expr expr'
  | CallI (expr, typs, args), CallI (expr', typs', args') ->
      eq_expr expr expr' && eq_list eq_type typs typs'
      && eq_list eq_arg args args'
  | TransI label, TransI label' -> eq_label label label'
  | SelectI (exprs, cases), SelectI (exprs', cases') ->
      eq_list eq_expr exprs exprs' && eq_list eq_select_case cases cases'
  | DeclI decl, DeclI decl' -> eq_decl decl decl'
  | _ -> false

and eq_block (block : block) (block' : block) =
  check "block" eq_block' pp_block (0, block) (0, block')

and eq_block' (_, block) (_, block') = eq_list eq_stmt block.it block'.it

(* Match-cases for switch *)

and eq_case (case : case) (case' : case) =
  check "case" eq_case' pp_case case case'

and eq_case' (case : case) (case' : case) =
  match (case.it, case'.it) with
  | CaseC case, CaseC case' -> case = case'
  | FallC case, FallC case' -> case = case'
  | DefaultC, DefaultC -> true
  | _ -> false

and eq_switch_case (switch_case : switch_case) (switch_case' : switch_case) =
  check "switch_case" eq_switch_case' pp_switch_case (0, switch_case)
    (0, switch_case')

and eq_switch_case' (_, switch_case) (_, switch_case') =
  let case, block = switch_case.it in
  let case', block' = switch_case'.it in
  eq_case case case' && eq_block block block'

(* Select-case for select *)

and eq_mtch (mtch : mtch) (mtch' : mtch) =
  check "mtch" eq_mtch' pp_mtch mtch mtch'

and eq_mtch' (mtch : mtch) (mtch' : mtch) =
  match (mtch.it, mtch'.it) with
  | ExprM expr, ExprM expr' -> eq_expr expr expr'
  | DefaultM, DefaultM -> true
  | AnyM, AnyM -> true
  | _ -> false

and eq_select_case (select_case : select_case) (select_case' : select_case) =
  check "select_case" eq_select_case' pp_select_case (0, select_case)
    (0, select_case')

and eq_select_case' (_, select_case) (_, select_case') =
  let mtchs, label = select_case.it in
  let mtchs', label' = select_case'.it in
  eq_list eq_mtch mtchs mtchs' && eq_label label label'

(* Declarations *)

and eq_decl (decl : decl) (decl' : decl) =
  check "decl" eq_decl' pp_decl (0, decl) (0, decl')

and eq_decl' (_, decl) (_, decl') =
  match (decl.it, decl'.it) with
  | ConstD { id; typ; value }, ConstD { id = id'; typ = typ'; value = value' }
    ->
      eq_id id id' && eq_type typ typ' && eq_expr value value'
  | VarD { id; typ; init }, VarD { id = id'; typ = typ'; init = init' } ->
      eq_id id id' && eq_type typ typ' && eq_option eq_expr init init'
  | ( InstD { id; typ; args; init },
      InstD { id = id'; typ = typ'; args = args'; init = init' } ) ->
      eq_id id id' && eq_type typ typ' && eq_list eq_arg args args'
      && eq_option eq_block init init'
  | ErrD { members }, ErrD { members = members' }
  | MatchKindD { members }, MatchKindD { members = members' } ->
      eq_list eq_member members members'
  | StructD { id; fields }, StructD { id = id'; fields = fields' }
  | HeaderD { id; fields }, HeaderD { id = id'; fields = fields' }
  | UnionD { id; fields }, UnionD { id = id'; fields = fields' } ->
      eq_id id id'
      && eq_list
           (fun (member, typ) (member', typ') ->
             eq_member member member' && eq_type typ typ')
           fields fields'
  | EnumD { id; members }, EnumD { id = id'; members = members' } ->
      eq_id id id' && eq_list eq_id members members'
  | ( SEnumD { id; typ; fields },
      SEnumD { id = id'; typ = typ'; fields = fields' } ) ->
      eq_id id id' && eq_type typ typ'
      && eq_list
           (fun (member, expr) (member', expr') ->
             eq_member member member' && eq_expr expr expr')
           fields fields'
  | NewTypeD { id; typdef }, NewTypeD { id = id'; typdef = typdef' }
  | TypeDefD { id; typdef }, TypeDefD { id = id'; typdef = typdef' } ->
      eq_id id id' && eq_alt eq_type eq_decl typdef typdef'
  | ( ValueSetD { id; typ; size },
      ValueSetD { id = id'; typ = typ'; size = size' } ) ->
      eq_id id id' && eq_type typ typ' && eq_expr size size'
  | ( ParserTypeD { id; tparams; params },
      ParserTypeD { id = id'; tparams = tparams'; params = params' } ) ->
      eq_id id id'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
  | ( ParserD { id; tparams; params; cparams; locals; states },
      ParserD
        {
          id = id';
          tparams = tparams';
          params = params';
          cparams = cparams';
          locals = locals';
          states = states';
        } ) ->
      eq_id id id'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
      && eq_list eq_cparam cparams cparams'
      && eq_list eq_decl locals locals'
      && eq_list eq_parser_state states states'
  | TableD { id; table }, TableD { id = id'; table = table' } ->
      eq_id id id' && eq_table table table'
  | ( ControlTypeD { id; tparams; params },
      ControlTypeD { id = id'; tparams = tparams'; params = params' } ) ->
      eq_id id id'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
  | ( ControlD { id; tparams; params; cparams; locals; body },
      ControlD
        {
          id = id';
          tparams = tparams';
          params = params';
          cparams = cparams';
          locals = locals';
          body = body';
        } ) ->
      eq_id id id'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
      && eq_list eq_cparam cparams cparams'
      && eq_list eq_decl locals locals'
      && eq_block body body'
  | ( ActionD { id; params; body },
      ActionD { id = id'; params = params'; body = body' } ) ->
      eq_id id id' && eq_list eq_param params params' && eq_block body body'
  | ( FuncD { id; typ_ret; tparams; params; body },
      FuncD
        {
          id = id';
          typ_ret = typ_ret';
          tparams = tparams';
          params = params';
          body = body';
        } ) ->
      eq_id id id' && eq_type typ_ret typ_ret'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
      && eq_block body body'
  | ( ExtFuncD { id; typ_ret; tparams; params },
      ExtFuncD
        { id = id'; typ_ret = typ_ret'; tparams = tparams'; params = params' } )
    ->
      eq_id id id' && eq_type typ_ret typ_ret'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
  | ( ExtConstructorD { id; cparams },
      ExtConstructorD { id = id'; cparams = cparams' } ) ->
      eq_id id id' && eq_list eq_cparam cparams cparams'
  | ( ExtAbstractMethodD { id; typ_ret; tparams; params },
      ExtAbstractMethodD
        { id = id'; typ_ret = typ_ret'; tparams = tparams'; params = params' } )
  | ( ExtMethodD { id; typ_ret; tparams; params },
      ExtMethodD
        { id = id'; typ_ret = typ_ret'; tparams = tparams'; params = params' } )
    ->
      eq_id id id' && eq_type typ_ret typ_ret'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
  | ( ExtObjectD { id; tparams; mthds },
      ExtObjectD { id = id'; tparams = tparams'; mthds = mthds' } ) ->
      eq_id id id'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_decl mthds mthds'
  | ( PackageTypeD { id; tparams; cparams },
      PackageTypeD { id = id'; tparams = tparams'; cparams = cparams' } ) ->
      eq_id id id'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_cparam cparams cparams'
  | _ -> false

and eq_parser_state (parser_state : parser_state) (parser_state' : parser_state)
    =
  check "parser_state" eq_parser_state' pp_parser_state (0, parser_state)
    (0, parser_state')

and eq_parser_state' (_, parser_state) (_, parser_state') =
  let label, block = parser_state.it in
  let label', block' = parser_state'.it in
  eq_label label label' && eq_block block block'

and eq_table_key (table_key : table_key) (table_key' : table_key) =
  check "table_key" eq_table_key' pp_table_key (0, table_key) (0, table_key')

and eq_table_key' (_, table_key) (_, table_key') =
  let expr, mtch_kind = table_key.it in
  let expr', mtch_kind' = table_key'.it in
  eq_expr expr expr' && eq_mtch_kind mtch_kind mtch_kind'

and eq_table_action (table_action : table_action) (table_action' : table_action)
    =
  check "table_action" eq_table_action' pp_table_action (0, table_action)
    (0, table_action')

and eq_table_action' (_, table_action) (_, table_action') =
  let var, args = table_action.it in
  let var', args' = table_action'.it in
  eq_var var var' && eq_list eq_arg args args'

and eq_table_entry (table_entry : table_entry) (table_entry' : table_entry) =
  check "table_entry" eq_table_entry' pp_table_entry (0, table_entry)
    (0, table_entry')

and eq_table_entry' (_, table_entry) (_, table_entry') =
  let mtchs, table_action = table_entry.it in
  let mtchs', table_action' = table_entry'.it in
  eq_list eq_mtch mtchs mtchs' && eq_table_action table_action table_action'

and eq_table_default (table_default : table_default)
    (table_default' : table_default) =
  check "table_default" eq_table_default' pp_table_default (0, table_default)
    (0, table_default')

and eq_table_default' (_, table_default) (_, table_default') =
  let table_action, const = table_default.it in
  let table_action', const' = table_default'.it in
  eq_table_action table_action table_action' && const = const'

and eq_table_custom (table_custom : table_custom) (table_custom' : table_custom)
    =
  check "table_custom" eq_table_custom' pp_table_custom (0, table_custom)
    (0, table_custom')

and eq_table_custom' (_, table_custom) (_, table_custom') =
  let member, expr, const = table_custom.it in
  let member', expr', const' = table_custom'.it in
  eq_member member member' && eq_expr expr expr' && const = const'

and eq_table (table : table) (table' : table) =
  check "table" eq_table' pp_table (0, table) (0, table')

and eq_table' (_, table) (_, table') =
  let key, actions, entries, default, custom = table in
  let key', actions', entries', default', custom' = table' in
  eq_list eq_table_key key key'
  && eq_list eq_table_action actions actions'
  && eq_list eq_table_entry entries entries'
  && eq_option eq_table_default default default'
  && eq_list eq_table_custom custom custom'

(* Program *)

let eq_program (program : program) (program' : program) =
  eq_list eq_decl program program'
