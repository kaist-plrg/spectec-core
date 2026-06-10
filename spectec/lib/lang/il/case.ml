open Types
open Common.Source

(* [?at] is unused for now (frontends pass [no_region]); kept so atoms can carry
   a source region once per-token positions are threaded through. *)
let atom ?(at = no_region) (a : Xl.Atom.t) : value Mixfix.mixeme =
  Mixfix.Atom (a $ at)

let arg (v : value) : value Mixfix.mixeme = Mixfix.Arg v
let kw ?at (id : string) : value Mixfix.mixeme = atom ?at (Xl.Atom.keyword id)
let tag ?at (id : string) : value Mixfix.mixeme = atom ?at (Xl.Atom.tag id)
let op ?at (s : string) : value Mixfix.mixeme = atom ?at (Xl.Atom.operator s)
let lparen ?at () : value Mixfix.mixeme = atom ?at Xl.Atom.LParen
let rparen ?at () : value Mixfix.mixeme = atom ?at Xl.Atom.RParen
let lbrack ?at () : value Mixfix.mixeme = atom ?at Xl.Atom.LBrack
let rbrack ?at () : value Mixfix.mixeme = atom ?at Xl.Atom.RBrack
let lbrace ?at () : value Mixfix.mixeme = atom ?at Xl.Atom.LBrace
let rbrace ?at () : value Mixfix.mixeme = atom ?at Xl.Atom.RBrace
let langle ?at () : value Mixfix.mixeme = atom ?at Xl.Atom.LAngle
let rangle ?at () : value Mixfix.mixeme = atom ?at Xl.Atom.RAngle

let case_v ~(var : string) (mixemes : value Mixfix.t) : value =
  CaseV mixemes |> Value.make_val (Typ.var var [])
