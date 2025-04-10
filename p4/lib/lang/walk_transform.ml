open Ast
open Util.Source

(* Parameterized walk types *)

type ('note_value,
       'note_expr,
       'typ,
       'value,
       'param,
       'expr,
       'stmt,
       'decl,
       'table_action,
       'table_entry,
       'mthd)
     transform_walker = {
  walk_num :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    num ->
    num;
  walk_text :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    text ->
    text;
  walk_id :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    id ->
    id;
  walk_var :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    var ->
    var;
  walk_member :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    member ->
    member;
  walk_state_label :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    state_label ->
    state_label;
  walk_match_kind :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    match_kind ->
    match_kind;
  walk_unop :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    unop ->
    unop;
  walk_binop :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    binop ->
    binop;
  walk_dir :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    dir ->
    dir;
  walk_typ :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'typ typ ->
    'typ typ;
  walk_value :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_value, 'value) value ->
    ('note_value, 'value) value;
  walk_anno :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr) anno ->
    ('note_expr, 'expr) anno;
  walk_tparam :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    tparam ->
    tparam;
  walk_param :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'param param ->
    'param param;
  walk_cparam :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'param cparam ->
    'param cparam;
  walk_targ :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'typ targ ->
    'typ targ;
  walk_arg :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr) arg ->
    ('note_expr, 'expr) arg;
  walk_expr :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr) expr ->
    ('note_expr, 'expr) expr;
  walk_keyset :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr) keyset ->
    ('note_expr, 'expr) keyset;
  walk_select_case :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr) select_case ->
    ('note_expr, 'expr) select_case;
  walk_stmt :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'stmt stmt ->
    'stmt stmt;
  walk_block :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr, 'stmt) block ->
    ('note_expr, 'expr, 'stmt) block;
  walk_switch_label :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr) switch_label ->
    ('note_expr, 'expr) switch_label;
  walk_switch_case :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr, 'stmt) switch_case ->
    ('note_expr, 'expr, 'stmt) switch_case;
  walk_decl :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'decl decl ->
    'decl decl;
  walk_parser_state :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr, 'stmt) parser_state ->
    ('note_expr, 'expr, 'stmt) parser_state;
  walk_table :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr, 'table_action, 'table_entry) table ->
    ('note_expr, 'expr, 'table_action, 'table_entry) table;
  walk_table_property :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr, 'table_action, 'table_entry) table_property ->
    ('note_expr, 'expr, 'table_action, 'table_entry) table_property;
  walk_table_keys :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr) table_keys ->
    ('note_expr, 'expr) table_keys;
  walk_table_key :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr) table_key ->
    ('note_expr, 'expr) table_key;
  walk_table_actions :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'table_action table_actions ->
    'table_action table_actions;
  walk_table_action :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'table_action table_action ->
    'table_action table_action;
  walk_table_entries :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'table_entry table_entries ->
    'table_entry table_entries;
  walk_table_entry :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'table_entry table_entry ->
    'table_entry table_entry;
  walk_table_default :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'table_action table_default ->
    'table_action table_default;
  walk_table_custom :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    ('note_expr, 'expr) table_custom ->
    ('note_expr, 'expr) table_custom;
  walk_mthd :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'mthd mthd ->
    'mthd mthd;
  walk_program :
    ( 'note_value,
      'note_expr,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    transform_walker ->
    'decl program ->
    'decl program;
}

(* Utility functions *)

let walk_alt (f : 'a -> 'a) (g : 'b -> 'b) (alt : ('a, 'b) alt) : ('a, 'b) alt =
  match alt with Left x -> Left (f x) | Right y -> Right (g y)

let walk_list (f : 'a -> 'a) (l : 'a list) : 'a list = List.map f l
let walk_option (f : 'a -> 'a) (o : 'a option) : 'a option = Option.map f o
let walk_pair (f_k : 'a -> 'a) (f_v : 'b -> 'b) (x, y) : 'a * 'b = (f_k x, f_v y)

(* Numbers *)

let walk_num
    (_walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) num =
  num

(* Texts *)

let walk_text
    (_walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) text =
  text

(* Identifiers *)

let walk_id
    (_walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) id =
  id

(* Variables (scoped identifiers) *)

let walk_var
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) var =
  let walk_id = walker.walk_id walker in
  let it =
    match var.it with
    | Top id -> Top (walk_id id)
    | Current id -> Current (walk_id id)
  in
  { var with it }

(* Members *)

let walk_member
    (_walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) member =
  member

(* State labels *)

let walk_state_label
    (_walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) state_label =
  state_label

(* Match kinds *)

let walk_match_kind
    (_walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) match_kind =
  match_kind

(* Unary operators *)

let walk_unop
    (_walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) unop =
  unop

(* Binary operators *)

let walk_binop
    (_walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) binop =
  binop

(* Directions *)

let walk_dir
    (_walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) dir =
  dir

(* Types *)

(* Values *)

(* Annotations *)

let walk_anno
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) anno =
  let walk_text = walker.walk_text walker in
  let walk_id = walker.walk_id walker in
  let walk_member = walker.walk_member walker in
  let walk_expr = walker.walk_expr walker in
  let it =
    match anno.it with
    | EmptyN text -> EmptyN (walk_text text)
    | TextN (text, texts) -> TextN (walk_text text, walk_list walk_text texts)
    | ExprN (id, exprs) -> ExprN (walk_id id, walk_list walk_expr exprs)
    | RecordN (id, fields) ->
        let walk_field (member, expr) =
          walk_pair walk_member walk_expr (member, expr)
        in
        RecordN (walk_id id, walk_list walk_field fields)
  in
  { anno with it }

(* Type parameters *)

let walk_tparam
    (_walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) tparam =
  tparam

(* Parameters *)

(* Constructor parameters *)

(* Type arguments *)

let walk_targ
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) targ =
  let walk_typ = walker.walk_typ walker in
  walk_typ targ

(* Arguments *)

let walk_arg
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) arg =
  let walk_id = walker.walk_id walker in
  let walk_expr = walker.walk_expr walker in
  let it =
    match arg.it with
    | ExprA expr -> ExprA (walk_expr expr)
    | NameA (id, expr) -> NameA (walk_id id, walk_option walk_expr expr)
    | AnyA -> AnyA
  in
  { arg with it }

(* Expressions *)

(* Keyset expressions *)

let walk_keyset
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) keyset =
  let walk_expr = walker.walk_expr walker in
  let it =
    match keyset.it with
    | ExprK expr -> ExprK (walk_expr expr)
    | DefaultK -> DefaultK
    | AnyK -> AnyK
  in
  { keyset with it }

(* Select-cases for select *)

let walk_select_case
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) select_case =
  let walk_state_label = walker.walk_state_label walker in
  let walk_keyset = walker.walk_keyset walker in
  let it =
    let keysets, state_label = select_case.it in
    let keysets = walk_list walk_keyset keysets in
    let state_label = walk_state_label state_label in
    (keysets, state_label)
  in
  { select_case with it }

(* Statements *)

(* Blocks (sequences of statements) *)

let walk_block
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) block =
  let walk_stmt = walker.walk_stmt walker in
  let it =
    let stmts, annos = block.it in
    let stmts = walk_list walk_stmt stmts in
    (stmts, annos)
  in
  { block with it }

(* Match-cases for switch *)

let walk_switch_label
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) switch_label =
  let walk_expr = walker.walk_expr walker in
  let it =
    match switch_label.it with
    | ExprL expr -> ExprL (walk_expr expr)
    | DefaultL -> DefaultL
  in
  { switch_label with it }

let walk_switch_case
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) switch_case =
  let walk_switch_label = walker.walk_switch_label walker in
  let walk_block = walker.walk_block walker in
  let it =
    match switch_case.it with
    | MatchC (switch_label, block) ->
        MatchC (walk_switch_label switch_label, walk_block block)
    | FallC switch_label -> FallC (walk_switch_label switch_label)
  in
  { switch_case with it }

(* Declarations *)

(* Parser states *)

let walk_parser_state
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) parser_state =
  let walk_state_label = walker.walk_state_label walker in
  let walk_block = walker.walk_block walker in
  let it =
    let state_label, block, annos = parser_state.it in
    let state_label = walk_state_label state_label in
    let block = walk_block block in
    (state_label, block, annos)
  in
  { parser_state with it }

(* Tables *)

let walk_table
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) table =
  let walk_table_property = walker.walk_table_property walker in
  walk_list walk_table_property table

(* Table properties *)

let walk_table_property
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) table_property =
  let walk_table_keys = walker.walk_table_keys walker in
  let walk_table_actions = walker.walk_table_actions walker in
  let walk_table_entries = walker.walk_table_entries walker in
  let walk_table_default = walker.walk_table_default walker in
  let walk_table_custom = walker.walk_table_custom walker in
  match table_property with
  | KeyP table_keys -> KeyP (walk_table_keys table_keys)
  | ActionP table_actions -> ActionP (walk_table_actions table_actions)
  | EntryP table_entries -> EntryP (walk_table_entries table_entries)
  | DefaultP table_default -> DefaultP (walk_table_default table_default)
  | CustomP table_custom -> CustomP (walk_table_custom table_custom)

(* Table keys *)

let walk_table_key
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) table_key =
  let walk_match_kind = walker.walk_match_kind walker in
  let walk_expr = walker.walk_expr walker in
  let it =
    let expr, match_kind, annos = table_key.it in
    let expr = walk_expr expr in
    let match_kind = walk_match_kind match_kind in
    (expr, match_kind, annos)
  in
  { table_key with it }

let walk_table_keys
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) table_keys =
  let walk_table_key = walker.walk_table_key walker in
  let it = walk_list walk_table_key table_keys.it in
  { table_keys with it }

(* Table actions *)

let walk_table_actions
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) table_actions =
  let walk_table_action = walker.walk_table_action walker in
  let it = walk_list walk_table_action table_actions.it in
  { table_actions with it }

(* Table entries *)

let walk_table_entries
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) table_entries =
  let walk_table_entry = walker.walk_table_entry walker in
  let it =
    let table_entries_const, table_entries_inner = table_entries.it in
    let table_entries_inner = walk_list walk_table_entry table_entries_inner in
    (table_entries_const, table_entries_inner)
  in
  { table_entries with it }

(* Table default properties *)

let walk_table_default
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) table_default =
  let walk_table_action = walker.walk_table_action walker in
  let it =
    let table_default_const, table_action = table_default.it in
    let table_action = walk_table_action table_action in
    (table_default_const, table_action)
  in
  { table_default with it }

(* Table custom properties *)

let walk_table_custom
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) table_custom =
  let walk_id = walker.walk_id walker in
  let walk_expr = walker.walk_expr walker in
  let it =
    let table_custom_const, id, expr, annos = table_custom.it in
    let id = walk_id id in
    let expr = walk_expr expr in
    (table_custom_const, id, expr, annos)
  in
  { table_custom with it }

(* Methods *)

(* Program *)

let walk_program
    (walker :
      ( 'note_value,
        'note_expr,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      transform_walker) program =
  let walk_decl = walker.walk_decl walker in
  walk_list walk_decl program
