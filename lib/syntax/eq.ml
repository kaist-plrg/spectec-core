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

(* Texts *)

let rec eq_text (text : text) (text' : text) =
  check "text" eq_text' pp_text text text'

and eq_text' (text : text) (text' : text) = text.it = text'.it

(* Identifiers *)

let rec eq_id (id : id) (id' : id) = check "id" eq_id' pp_id id id'
and eq_id' (id : id) (id' : id) = id.it = id'.it

(* Paths *)

let rec eq_path (path : path) (path' : path) =
  check "path" eq_path' pp_path path path'

and eq_path' (path : path) (path' : path) = List.for_all2 eq_id path path'

(* Variables (scoped identifiers) *)

let rec eq_var (var : var) (var' : var) = check "var" eq_var' pp_var var var'

and eq_var' (var : var) (var' : var) =
  match (var.it, var'.it) with
  | Top id, Top id' -> eq_id id id'
  | Current id, Current id' -> eq_id id id'
  | _ -> false

(* Members (field names) *)

let rec eq_member (member : member) (member' : member) =
  check "member" eq_member' pp_member member member'

and eq_member' (member : member) (member' : member) = member.it = member'.it

(* State labels *)

let rec eq_state_label (state_label : state_label) (state_label' : state_label)
    =
  check "label" eq_state_label' pp_state_label state_label state_label'

and eq_state_label' (state_label : state_label) (state_label' : state_label) =
  state_label.it = state_label'.it

(* Match kinds *)

let rec eq_match_kind (match_kind : match_kind) (match_kind' : match_kind) =
  check "match_kind" eq_match_kind' pp_match_kind match_kind match_kind'

and eq_match_kind' (match_kind : match_kind) (match_kind' : match_kind) =
  match_kind.it = match_kind'.it

(* Unary operators *)

let rec eq_unop (unop : unop) (unop' : unop) =
  check "unop" eq_unop' pp_unop unop unop'

and eq_unop' (unop : unop) (unop' : unop) = unop.it = unop'.it

(* Binary operators *)

let rec eq_binop (binop : binop) (binop' : binop) =
  check "binop" eq_binop' pp_binop binop binop'

and eq_binop' (binop : binop) (binop' : binop) = binop.it = binop'.it

(* Annotations *)

let rec eq_anno (anno : anno) (anno' : anno) =
  check "anno" eq_anno' pp_anno anno anno'

and eq_anno' (anno : anno) (anno' : anno) =
  match (anno.it, anno'.it) with
  | EmptyN text_name, EmptyN text_name' -> eq_text text_name text_name'
  | TextN (text_name, texts), TextN (text_name', texts') ->
      eq_text text_name text_name' && eq_list eq_text texts texts'
  | ExprN (text_name, exprs), ExprN (text_name', exprs') ->
      eq_text text_name text_name' && eq_list eq_expr exprs exprs'
  | RecordN (text_name, fields), RecordN (text_name', fields') ->
      eq_text text_name text_name'
      && eq_list
           (fun (member, expr) (member', expr') ->
             eq_member member member' && eq_expr expr expr')
           fields fields'
  | _ -> false

(* Types *)

and eq_type (typ : typ) (typ' : typ) = check "typ" eq_type' pp_type typ typ'

and eq_type' (typ : typ) (typ' : typ) =
  match (typ.it, typ'.it) with
  | VoidT, VoidT -> true
  | BoolT, BoolT -> true
  | MatchKindT, MatchKindT -> true
  | ErrT, ErrT -> true
  | StrT, StrT -> true
  | IntT, IntT -> true
  | FIntT expr, FIntT expr' -> eq_expr expr expr'
  | FBitT expr, FBitT expr' -> eq_expr expr expr'
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

(* Type parameters *)

and eq_tparam (tparam : tparam) (tparam' : tparam) =
  check "tparam" eq_tparam' pp_tparam tparam tparam'

and eq_tparam' (tparam : tparam) (tparam' : tparam) = tparam.it = tparam'.it

(* Parameters *)

and eq_param (param : param) (param' : param) =
  check "param" eq_param' pp_param param param'

and eq_param' (param : param) (param' : param) =
  let id, dir, typ, expr, annos = param.it in
  let id', dir', typ', expr', annos' = param'.it in
  eq_id id id' && eq_dir dir dir' && eq_type typ typ'
  && eq_option eq_expr expr expr'
  && eq_list eq_anno annos annos'

(* Constructor parameters *)

and eq_cparam (cparam : cparam) (cparam' : cparam) =
  check "cparam" eq_param' pp_cparam cparam cparam'

(* Type arguments *)

and eq_targ (targ : targ) (targ' : targ) =
  check "targ" eq_type pp_targ targ targ'

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
  | StrE t, StrE t' -> eq_text t t'
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
  | CallE (expr, targs, args), CallE (expr', targs', args') ->
      eq_expr expr expr'
      && eq_list eq_targ targs targs'
      && eq_list eq_arg args args'
  | InstE (typ, args), InstE (typ', args') ->
      eq_type typ typ' && eq_list eq_arg args args'
  | _ -> false

(* Keyset expressions *)

and eq_keyset (keyset : keyset) (keyset' : keyset) =
  check "keyset" eq_keyset' pp_keyset keyset keyset'

and eq_keyset' (keyset : keyset) (keyset' : keyset) =
  match (keyset.it, keyset'.it) with
  | ExprK expr, ExprK expr' -> eq_expr expr expr'
  | DefaultK, DefaultK -> true
  | AnyK, AnyK -> true
  | _ -> false

(* Statements *)

and eq_stmt (stmt : stmt) (stmt' : stmt) =
  check "stmt" eq_stmt' pp_stmt stmt stmt'

and eq_stmt' stmt stmt' =
  match (stmt.it, stmt'.it) with
  | EmptyS, EmptyS -> true
  | AssignS (expr_l, expr_r), AssignS (expr_l', expr_r') ->
      eq_expr expr_l expr_l' && eq_expr expr_r expr_r'
  | SwitchS (expr, cases), SwitchS (expr', cases') ->
      eq_expr expr expr' && eq_list eq_switch_case cases cases'
  | IfS (expr, stmt_then, stmt_else), IfS (expr', stmt_then', stmt_else') ->
      eq_expr expr expr'
      && eq_stmt stmt_then stmt_then'
      && eq_stmt stmt_else stmt_else'
  | BlockS block, BlockS block' -> eq_block block block'
  | ExitS, ExitS -> true
  | RetS expr, RetS expr' -> eq_option eq_expr expr expr'
  | CallS (expr, targs, args), CallS (expr', targs', args') ->
      eq_expr expr expr'
      && eq_list eq_targ targs targs'
      && eq_list eq_arg args args'
  | TransS state_label, TransS state_label' ->
      eq_state_label state_label state_label'
  | SelectS (exprs, cases), SelectS (exprs', cases') ->
      eq_list eq_expr exprs exprs' && eq_list eq_select_case cases cases'
  | DeclS decl, DeclS decl' -> eq_decl decl decl'
  | _ -> false

(* Blocks (sequence of statements) *)

and eq_block (block : block) (block' : block) =
  check "block" eq_block' pp_block block block'

and eq_block' block block' =
  let stmts, annos = block.it in
  let stmts', annos' = block'.it in
  eq_list eq_stmt stmts stmts' && eq_list eq_anno annos annos'

(* Match-cases for switch *)

and eq_switch_label (switch_label : switch_label) (switch_label' : switch_label)
    =
  check "switch_label" eq_switch_label' pp_switch_label switch_label
    switch_label'

and eq_switch_label' switch_label switch_label' =
  match (switch_label.it, switch_label'.it) with
  | NameL t, NameL t' -> eq_text t t'
  | DefaultL, DefaultL -> true
  | _ -> false

and eq_switch_case (switch_case : switch_case) (switch_case' : switch_case) =
  check "switch_case" eq_switch_case' pp_switch_case switch_case switch_case'

and eq_switch_case' switch_case switch_case' =
  match (switch_case.it, switch_case'.it) with
  | MatchC (switch_label, block), MatchC (switch_label', block') ->
      eq_switch_label switch_label switch_label' && eq_block block block'
  | FallC switch_label, FallC switch_label' ->
      eq_switch_label switch_label switch_label'
  | _ -> false

(* Select-case for select *)

and eq_select_case (select_case : select_case) (select_case' : select_case) =
  check "select_case" eq_select_case' pp_select_case select_case select_case'

and eq_select_case' select_case select_case' =
  let keysets, state_label = select_case.it in
  let keysets', state_label' = select_case'.it in
  eq_list eq_keyset keysets keysets' && eq_state_label state_label state_label'

(* Declarations *)

and eq_decl (decl : decl) (decl' : decl) =
  check "decl" eq_decl' pp_decl decl decl'

and eq_decl' decl decl' =
  match (decl.it, decl'.it) with
  | ( ConstD { id; typ; value; annos },
      ConstD { id = id'; typ = typ'; value = value'; annos = annos' } ) ->
      eq_id id id' && eq_type typ typ' && eq_expr value value'
      && eq_list eq_anno annos annos'
  | ( VarD { id; typ; init; annos },
      VarD { id = id'; typ = typ'; init = init'; annos = annos' } ) ->
      eq_id id id' && eq_type typ typ'
      && eq_option eq_expr init init'
      && eq_list eq_anno annos annos'
  | ( InstD { id; typ; args; init; annos },
      InstD { id = id'; typ = typ'; args = args'; init = init'; annos = annos' }
    ) ->
      eq_id id id' && eq_type typ typ' && eq_list eq_arg args args'
      && eq_option eq_block init init'
      && eq_list eq_anno annos annos'
  | ErrD { members }, ErrD { members = members' }
  | MatchKindD { members }, MatchKindD { members = members' } ->
      eq_list eq_member members members'
  | ( StructD { id; fields; annos },
      StructD { id = id'; fields = fields'; annos = annos' } )
  | ( HeaderD { id; fields; annos },
      HeaderD { id = id'; fields = fields'; annos = annos' } )
  | ( UnionD { id; fields; annos },
      UnionD { id = id'; fields = fields'; annos = annos' } ) ->
      eq_id id id'
      && eq_list
           (fun (member, typ, annos) (member', typ', annos') ->
             eq_member member member' && eq_type typ typ'
             && eq_list eq_anno annos annos')
           fields fields'
      && eq_list eq_anno annos annos'
  | ( EnumD { id; members; annos },
      EnumD { id = id'; members = members'; annos = annos' } ) ->
      eq_id id id'
      && eq_list eq_id members members'
      && eq_list eq_anno annos annos'
  | ( SEnumD { id; typ; fields; annos },
      SEnumD { id = id'; typ = typ'; fields = fields'; annos = annos' } ) ->
      eq_id id id' && eq_type typ typ'
      && eq_list
           (fun (member, expr) (member', expr') ->
             eq_member member member' && eq_expr expr expr')
           fields fields'
      && eq_list eq_anno annos annos'
  | ( NewTypeD { id; typ; annos },
      NewTypeD { id = id'; typ = typ'; annos = annos' } )
  | ( TypeDefD { id; typ; annos },
      TypeDefD { id = id'; typ = typ'; annos = annos' } ) ->
      eq_id id id'
      && eq_alt eq_type eq_decl typ typ'
      && eq_list eq_anno annos annos'
  | ( ValueSetD { id; typ; size; annos },
      ValueSetD { id = id'; typ = typ'; size = size'; annos = annos' } ) ->
      eq_id id id' && eq_type typ typ' && eq_expr size size'
      && eq_list eq_anno annos annos'
  | ( ParserTypeD { id; tparams; params; annos },
      ParserTypeD
        { id = id'; tparams = tparams'; params = params'; annos = annos' } ) ->
      eq_id id id'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
      && eq_list eq_anno annos annos'
  | ( ParserD { id; tparams; params; cparams; locals; states; annos },
      ParserD
        {
          id = id';
          tparams = tparams';
          params = params';
          cparams = cparams';
          locals = locals';
          states = states';
          annos = annos';
        } ) ->
      eq_id id id'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
      && eq_list eq_cparam cparams cparams'
      && eq_list eq_decl locals locals'
      && eq_list eq_parser_state states states'
      && eq_list eq_anno annos annos'
  | ( TableD { id; table; annos },
      TableD { id = id'; table = table'; annos = annos' } ) ->
      eq_id id id' && eq_table table table' && eq_list eq_anno annos annos'
  | ( ControlTypeD { id; tparams; params; annos },
      ControlTypeD
        { id = id'; tparams = tparams'; params = params'; annos = annos' } ) ->
      eq_id id id'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
      && eq_list eq_anno annos annos'
  | ( ControlD { id; tparams; params; cparams; locals; body; annos },
      ControlD
        {
          id = id';
          tparams = tparams';
          params = params';
          cparams = cparams';
          locals = locals';
          body = body';
          annos = annos';
        } ) ->
      eq_id id id'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
      && eq_list eq_cparam cparams cparams'
      && eq_list eq_decl locals locals'
      && eq_block body body'
      && eq_list eq_anno annos annos'
  | ( ActionD { id; params; body; annos },
      ActionD { id = id'; params = params'; body = body'; annos = annos' } ) ->
      eq_id id id'
      && eq_list eq_param params params'
      && eq_block body body'
      && eq_list eq_anno annos annos'
  | ( FuncD { id; rettyp; tparams; params; body },
      FuncD
        {
          id = id';
          rettyp = rettyp';
          tparams = tparams';
          params = params';
          body = body';
        } ) ->
      eq_id id id' && eq_type rettyp rettyp'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
      && eq_block body body'
  | ( ExternFuncD { id; rettyp; tparams; params; annos },
      ExternFuncD
        {
          id = id';
          rettyp = rettyp';
          tparams = tparams';
          params = params';
          annos = annos';
        } ) ->
      eq_id id id' && eq_type rettyp rettyp'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
      && eq_list eq_anno annos annos'
  | ( ConsD { id; cparams; annos },
      ConsD { id = id'; cparams = cparams'; annos = annos' } ) ->
      eq_id id id'
      && eq_list eq_cparam cparams cparams'
      && eq_list eq_anno annos annos'
  | ( AbstractD { id; rettyp; tparams; params; annos },
      AbstractD
        {
          id = id';
          rettyp = rettyp';
          tparams = tparams';
          params = params';
          annos = annos';
        } )
  | ( MethodD { id; rettyp; tparams; params; annos },
      MethodD
        {
          id = id';
          rettyp = rettyp';
          tparams = tparams';
          params = params';
          annos = annos';
        } ) ->
      eq_id id id' && eq_type rettyp rettyp'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_param params params'
      && eq_list eq_anno annos annos'
  | ( ExternObjectD { id; tparams; mthds; annos },
      ExternObjectD
        { id = id'; tparams = tparams'; mthds = mthds'; annos = annos' } ) ->
      eq_id id id'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_decl mthds mthds'
      && eq_list eq_anno annos annos'
  | ( PackageTypeD { id; tparams; cparams; annos },
      PackageTypeD
        { id = id'; tparams = tparams'; cparams = cparams'; annos = annos' } )
    ->
      eq_id id id'
      && eq_list eq_tparam tparams tparams'
      && eq_list eq_cparam cparams cparams'
      && eq_list eq_anno annos annos'
  | _ -> false

(* Parser state machine *)

and eq_parser_state (parser_state : parser_state) (parser_state' : parser_state)
    =
  check "parser_state" eq_parser_state' pp_parser_state parser_state
    parser_state'

and eq_parser_state' parser_state parser_state' =
  let state_label, block, annos = parser_state.it in
  let state_label', block', annos' = parser_state'.it in
  eq_state_label state_label state_label'
  && eq_block block block'
  && eq_list eq_anno annos annos'

(* Tables *)

and eq_table (table : table) (table' : table) =
  check "table" eq_table' pp_table table table'

and eq_table' table table' =
  let table_keys, table_actions, table_entries, table_default, table_custom =
    table
  in
  let table_keys', table_actions', table_entries', table_default', table_custom'
      =
    table'
  in
  eq_list eq_table_key table_keys table_keys'
  && eq_list eq_table_action table_actions table_actions'
  && eq_list eq_table_entry table_entries table_entries'
  && eq_option eq_table_default table_default table_default'
  && eq_list eq_table_custom table_custom table_custom'

(* Table keys *)

and eq_table_key (table_key : table_key) (table_key' : table_key) =
  check "table_key" eq_table_key' pp_table_key table_key table_key'

and eq_table_key' table_key table_key' =
  let expr, match_kind, annos = table_key.it in
  let expr', match_kind', annos' = table_key'.it in
  eq_expr expr expr'
  && eq_match_kind match_kind match_kind'
  && eq_list eq_anno annos annos'

(* Table action references *)

and eq_table_action (table_action : table_action) (table_action' : table_action)
    =
  check "table_action" eq_table_action' pp_table_action table_action
    table_action'

and eq_table_action' table_action table_action' =
  let var, args, annos = table_action.it in
  let var', args', annos' = table_action'.it in
  eq_var var var' && eq_list eq_arg args args' && eq_list eq_anno annos annos'

(* Table entries *)

and eq_table_entry (table_entry : table_entry) (table_entry' : table_entry) =
  check "table_entry" eq_table_entry' pp_table_entry table_entry table_entry'

and eq_table_entry' table_entry table_entry' =
  let keysets, table_action, annos = table_entry.it in
  let keysets', table_action', annos' = table_entry'.it in
  eq_list eq_keyset keysets keysets'
  && eq_table_action table_action table_action'
  && eq_list eq_anno annos annos'

(* Table default properties *)

and eq_table_default (table_default : table_default)
    (table_default' : table_default) =
  check "table_default" eq_table_default' pp_table_default table_default
    table_default'

and eq_table_default' table_default table_default' =
  let table_action, table_default_const = table_default.it in
  let table_action', table_default_const' = table_default'.it in
  eq_table_action table_action table_action'
  && table_default_const = table_default_const'

(* Table custom properties *)

and eq_table_custom (table_custom : table_custom) (table_custom' : table_custom)
    =
  check "table_custom" eq_table_custom' pp_table_custom table_custom
    table_custom'

and eq_table_custom' table_custom table_custom' =
  let member, expr, table_custom_const, annos = table_custom.it in
  let member', expr', table_custom_const', annos' = table_custom'.it in
  eq_member member member' && eq_expr expr expr'
  && table_custom_const = table_custom_const'
  && eq_list eq_anno annos annos'

(* Program *)

let eq_program (program : program) (program' : program) =
  eq_list eq_decl program program'
