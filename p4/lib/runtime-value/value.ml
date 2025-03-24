include Vdom

let pp = Pp.pp
let eq = Eq.eq
let get_width = Utils.get_width
let get_bool = Utils.get_bool
let get_num = Utils.get_num
let get_tuple = Utils.get_tuple
let get_seq = Utils.get_seq
let get_tuple_or_seq = Utils.get_tuple_or_seq
let get_struct = Utils.get_struct
let get_struct_field = Utils.get_struct_field
let get_header = Utils.get_header
let get_header_valid = Utils.get_header_valid
let get_header_field = Utils.get_header_field
let get_state = Utils.get_state
let set_header_invalid = Utils.set_header_invalid
let set_union_invalid = Utils.set_union_invalid
let set_invalid = Utils.set_invalid
let update_struct_field = Utils.update_struct_field
let update_header_field = Utils.update_header_field
let update_union_field = Utils.update_union_field
