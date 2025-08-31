open Ast
open P4util.Source

(* Parameterized walk types *)

type ('note,
       'typ,
       'value,
       'param,
       'expr,
       'stmt,
       'decl,
       'table_action,
       'table_entry,
       'mthd)
     walker = {
  walk_num :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    num ->
    unit;
  walk_text :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    text ->
    unit;
  walk_id :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    id ->
    unit;
  walk_var :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    var ->
    unit;
  walk_member :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    member ->
    unit;
  walk_state_label :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    state_label ->
    unit;
  walk_match_kind :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    match_kind ->
    unit;
  walk_unop :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    unop ->
    unit;
  walk_binop :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    binop ->
    unit;
  walk_dir :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    dir ->
    unit;
  walk_typ :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'typ typ ->
    unit;
  walk_value :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'value value ->
    unit;
  walk_anno :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr) anno ->
    unit;
  walk_tparam :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    tparam ->
    unit;
  walk_param :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'param param ->
    unit;
  walk_cparam :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'param cparam ->
    unit;
  walk_targ :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'typ targ ->
    unit;
  walk_arg :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr) arg ->
    unit;
  walk_expr :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr) expr ->
    unit;
  walk_keyset :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr) keyset ->
    unit;
  walk_select_case :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr) select_case ->
    unit;
  walk_stmt :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'stmt stmt ->
    unit;
  walk_block :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr, 'stmt) block ->
    unit;
  walk_switch_label :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr) switch_label ->
    unit;
  walk_switch_case :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr, 'stmt) switch_case ->
    unit;
  walk_decl :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'decl decl ->
    unit;
  walk_parser_state :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr, 'stmt) parser_state ->
    unit;
  walk_table :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr, 'table_action, 'table_entry) table ->
    unit;
  walk_table_property :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr, 'table_action, 'table_entry) table_property ->
    unit;
  walk_table_keys :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr) table_keys ->
    unit;
  walk_table_key :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr) table_key ->
    unit;
  walk_table_actions :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'table_action table_actions ->
    unit;
  walk_table_action :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'table_action table_action ->
    unit;
  walk_table_entries :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'table_entry table_entries ->
    unit;
  walk_table_entry :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'table_entry table_entry ->
    unit;
  walk_table_default :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'table_action table_default ->
    unit;
  walk_table_custom :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    ('note, 'expr) table_custom ->
    unit;
  walk_mthd :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'mthd mthd ->
    unit;
  walk_program :
    ( 'note,
      'typ,
      'value,
      'param,
      'expr,
      'stmt,
      'decl,
      'table_action,
      'table_entry,
      'mthd )
    walker ->
    'decl program ->
    unit;
}

(* Utility functions *)

let walk_alt (f : 'a -> unit) (g : 'b -> unit) (alt : ('a, 'b) alt) : unit =
  match alt with Left x -> f x | Right y -> g y

let walk_list (f : 'a -> unit) (l : 'a list) : unit = List.iter f l

let walk_option (f : 'a -> unit) (o : 'a option) : unit =
  match o with Some x -> f x | None -> ()

let walk_pair (f_k : 'a -> unit) (f_v : 'b -> unit) (x, y) : unit =
  f_k x;
  f_v y

(* Numbers *)

let walk_num
    (_walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) _num =
  ()

(* Texts *)

let walk_text
    (_walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) _text =
  ()

(* Identifiers *)

let walk_id
    (_walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) _id =
  ()

(* Variables (scoped identifiers) *)

let walk_var
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) var =
  let walk_id = walker.walk_id walker in
  match var.it with Top id | Current id -> walk_id id

(* Members *)

let walk_member
    (_walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) _member =
  ()

(* State labels *)

let walk_state_label
    (_walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) _state_label =
  ()

(* Match kinds *)

let walk_match_kind
    (_walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) _match_kind =
  ()

(* Unary operators *)

let walk_unop
    (_walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) _unop =
  ()

(* Binary operators *)

let walk_binop
    (_walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) _binop =
  ()

(* Directions *)

let walk_dir
    (_walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) _dir =
  ()

(* Types *)

(* Values *)

(* Annotations *)

let walk_anno
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) anno =
  let walk_text = walker.walk_text walker in
  let walk_id = walker.walk_id walker in
  let walk_member = walker.walk_member walker in
  let walk_expr = walker.walk_expr walker in
  match anno.it with
  | EmptyN text -> walk_text text
  | TextN (text, texts) ->
      walk_text text;
      walk_list walk_text texts
  | ExprN (id, exprs) ->
      walk_id id;
      walk_list walk_expr exprs
  | RecordN (id, fields) ->
      let walk_field (member, expr) =
        walk_pair walk_member walk_expr (member, expr)
      in
      walk_id id;
      walk_list walk_field fields

(* Type parameters *)

let walk_tparam
    (_walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) _tparam =
  ()

(* Parameters *)

(* Constructor parameters *)

(* Type arguments *)

let walk_targ
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) targ =
  let walk_typ = walker.walk_typ walker in
  walk_typ targ

(* Arguments *)

let walk_arg
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) arg =
  let walk_id = walker.walk_id walker in
  let walk_expr = walker.walk_expr walker in
  match arg.it with
  | ExprA expr -> walk_expr expr
  | NameA (id, expr) ->
      walk_id id;
      walk_option walk_expr expr
  | AnyA -> ()

(* Expressions *)

(* Keyset expressions *)

let walk_keyset
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) keyset =
  let walk_expr = walker.walk_expr walker in
  match keyset.it with ExprK expr -> walk_expr expr | DefaultK | AnyK -> ()

(* Select-cases for select *)

let walk_select_case
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) select_case =
  let walk_state_label = walker.walk_state_label walker in
  let walk_keyset = walker.walk_keyset walker in
  let keysets, state_label = select_case.it in
  walk_list walk_keyset keysets;
  walk_state_label state_label

(* Statements *)

(* Blocks (sequences of statements) *)

let walk_block
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) block =
  let walk_stmt = walker.walk_stmt walker in
  let stmts, _annos = block.it in
  walk_list walk_stmt stmts

(* Match-cases for switch *)

let walk_switch_label
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) switch_label =
  let walk_expr = walker.walk_expr walker in
  match switch_label.it with ExprL expr -> walk_expr expr | DefaultL -> ()

let walk_switch_case
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) switch_case =
  let walk_switch_label = walker.walk_switch_label walker in
  let walk_block = walker.walk_block walker in
  match switch_case.it with
  | MatchC (switch_label, block) ->
      walk_switch_label switch_label;
      walk_block block
  | FallC switch_label -> walk_switch_label switch_label

(* Declarations *)

(* Parser states *)

let walk_parser_state
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) parser_state =
  let walk_state_label = walker.walk_state_label walker in
  let walk_block = walker.walk_block walker in
  let state_label, block, _annos = parser_state.it in
  walk_state_label state_label;
  walk_block block

(* Tables *)

let walk_table
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) table =
  let walk_table_property = walker.walk_table_property walker in
  walk_list walk_table_property table

(* Table properties *)

let walk_table_property
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) table_property =
  let walk_table_keys = walker.walk_table_keys walker in
  let walk_table_actions = walker.walk_table_actions walker in
  let walk_table_entries = walker.walk_table_entries walker in
  let walk_table_default = walker.walk_table_default walker in
  let walk_table_custom = walker.walk_table_custom walker in
  match table_property with
  | KeyP table_keys -> walk_table_keys table_keys
  | ActionP table_actions -> walk_table_actions table_actions
  | EntryP table_entries -> walk_table_entries table_entries
  | DefaultP table_default -> walk_table_default table_default
  | CustomP table_custom -> walk_table_custom table_custom

(* Table keys *)

let walk_table_key
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) table_key =
  let walk_match_kind = walker.walk_match_kind walker in
  let walk_expr = walker.walk_expr walker in
  let expr, match_kind, _annos = table_key.it in
  walk_expr expr;
  walk_match_kind match_kind

let walk_table_keys
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) table_keys =
  let walk_table_key = walker.walk_table_key walker in
  walk_list walk_table_key table_keys.it

(* Table actions *)

let walk_table_actions
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) table_actions =
  let walk_table_action = walker.walk_table_action walker in
  walk_list walk_table_action table_actions.it

(* Table entries *)

let walk_table_entries
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) table_entries =
  let walk_table_entry = walker.walk_table_entry walker in
  let _table_entries_const, table_entries = table_entries.it in
  walk_list walk_table_entry table_entries

(* Table default properties *)

let walk_table_default
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) table_default =
  let walk_table_action = walker.walk_table_action walker in
  let _table_default_const, table_action = table_default.it in
  walk_table_action table_action

(* Table custom properties *)

let walk_table_custom
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) table_custom =
  let walk_id = walker.walk_id walker in
  let walk_expr = walker.walk_expr walker in
  let _table_custom_const, id, expr, _annos = table_custom.it in
  walk_id id;
  walk_expr expr

(* Methods *)

(* Program *)

let walk_program
    (walker :
      ( 'note,
        'typ,
        'value,
        'param,
        'expr,
        'stmt,
        'decl,
        'table_action,
        'table_entry,
        'mthd )
      walker) program =
  let walk_decl = walker.walk_decl walker in
  walk_list walk_decl program
