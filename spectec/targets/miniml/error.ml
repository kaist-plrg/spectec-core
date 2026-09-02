open Common.Source

exception MinimlParseError of region * string

let error (at : region) (msg : string) = raise (MinimlParseError (at, msg))
