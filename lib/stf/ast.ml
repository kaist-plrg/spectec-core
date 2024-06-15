type name = string
type id = string
type number = string
type port = string
type packet = string
type expect = string
type arg = id * number
type action = name * arg list
type mtchkind = Num of number | Slash of number * number
type mtch = name * mtchkind
type id_or_index = Id of string | Index of number
type cond = Eq | Ne | Le | Lt | Ge | Gt
type ctr = Bytes | Packets

type stmt =
  | Wait
  | RemoveAll
  | Expect of port * expect option
  | Packet of port * packet
  | NoPacket
  | Add of name * int option * mtch list * action * id option
  | SetDefault of name * action
  | CheckCounter of id * id_or_index * (ctr option * cond * number)
