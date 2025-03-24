module F = Format
module Value = Runtime_value.Value
module L = Il.Ast
module P = Il.Pp

type t =
  | VarLV of L.var'
  | BitAccLV of t * Value.t * Value.t
  | ArrAccLV of t * Value.t
  | ExprAccLV of t * L.member'

let rec pp ?(level = 0) fmt lvalue =
  match lvalue with
  | VarLV var -> F.fprintf fmt "%a" P.pp_var' var
  | BitAccLV (lvalue_base, idx_lo, idx_hi) ->
      F.fprintf fmt "%a[%a:%a]" (pp ~level) lvalue_base (Value.pp ~level) idx_hi
        (Value.pp ~level) idx_lo
  | ArrAccLV (lvalue_base, idx) ->
      F.fprintf fmt "%a[%a]" (pp ~level) lvalue_base (Value.pp ~level) idx
  | ExprAccLV (lvalue_base, member) ->
      F.fprintf fmt "%a.%a" (pp ~level) lvalue_base P.pp_member' member
