open Common.Source
module El = Lang.El

type kind = Syntax | Var | Relation | Dec | Rule | Case | Field
type part = Literal of string | Hole of string

type entry = {
  name : string;
  kind : kind;
  region : region;
  signature : string;
  detail : string option;
  doc : string option;
  shape : part list;
  fills : string option;
  notation : part list;
}

type t = entry list

let empty = []

let string_of_kind = function
  | Syntax -> "syntax"
  | Var -> "var"
  | Relation -> "relation"
  | Dec -> "dec"
  | Rule -> "rule"
  | Case -> "case"
  | Field -> "field"

let with_tparams id tparams = id ^ El.Print.string_of_tparams tparams

(* Keep hover signatures compact while retaining variant cases. *)
let signature_limit = 110

let one_line text =
  let buffer = Buffer.create (String.length text) in
  let space_pending = ref false in
  String.iter
    (fun c ->
      match c with
      | ' ' | '\t' | '\n' | '\r' ->
          if Buffer.length buffer > 0 then space_pending := true
      | c ->
          if !space_pending then Buffer.add_char buffer ' ';
          space_pending := false;
          Buffer.add_char buffer c)
    text;
  Buffer.contents buffer

let summarise text =
  let text = one_line text in
  if String.length text <= signature_limit then text
  else String.sub text 0 signature_limit ^ " …"

let is_name_start c =
  ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c = '_'

let is_name_char c = is_name_start c || ('0' <= c && c <= '9') || c = '\''

(* Only word atoms, including underscore tags, support hovering. *)
let atom_name (atom : El.atom) =
  let text = El.Print.string_of_atom atom in
  if
    String.length text > 0
    && is_name_start text.[0]
    && String.for_all is_name_char text
  then Some text
  else None

(* Convert types into holes and atoms into literals. *)
let rec parts_of_typ (typ : El.typ) : part list =
  match typ with
  | El.PlainT plaintyp -> [ Hole (El.Print.string_of_plaintyp plaintyp) ]
  | El.NotationT nottyp -> parts_of_nottyp nottyp

and parts_of_nottyp (nottyp : El.nottyp) : part list =
  match nottyp.it with
  | El.AtomT atom -> [ Literal (El.Print.string_of_atom atom) ]
  (* Omit grouping braces absent from use sites. *)
  | El.SeqT typs -> joined " " (List.map parts_of_typ typs)
  | El.InfixT (typ_l, atom, typ_r) ->
      let left = parts_of_typ typ_l and right = parts_of_typ typ_r in
      let atom = El.Print.string_of_atom atom in
      (* Avoid extra spaces beside empty notation operands. *)
      let separator =
        (if left = [] then atom else " " ^ atom)
        ^ if right = [] then "" else " "
      in
      (left @ [ Literal separator ]) @ right
  | El.BrackT (atom_l, typ, atom_r) ->
      (Literal (El.Print.string_of_atom atom_l) :: parts_of_typ typ)
      @ [ Literal (El.Print.string_of_atom atom_r) ]

and joined separator (parts : part list list) : part list =
  match parts with
  | [] -> []
  | first :: rest ->
      first @ List.concat_map (fun parts -> Literal separator :: parts) rest

(* Discard shapes with no arguments to fill. *)
let shape (parts : part list) : part list =
  if List.exists (function Hole _ -> true | Literal _ -> false) parts then parts
  else []

(* Parenthesise nonempty function arguments, separated by commas. *)
let dec_shape name (params : El.param list) : part list =
  match params with
  | [] -> [ Literal name ]
  | params ->
      Literal (name ^ "(")
      :: joined ", "
           (List.map
              (fun param -> [ Hole (El.Print.string_of_param param) ])
              params)
      @ [ Literal ")" ]

(* Extract constructor atoms; bare types declare no case. *)
let case_head (typ : El.typ) =
  match typ with
  | El.PlainT _ -> None
  | El.NotationT nottyp -> (
      match nottyp.it with
      | El.AtomT atom ->
          Option.map (fun name -> (name, atom, [])) (atom_name atom)
      | El.SeqT (El.NotationT head :: rest) -> (
          match head.it with
          | El.AtomT atom ->
              Option.map (fun name -> (name, atom, rest)) (atom_name atom)
          | _ -> None)
      | _ -> None)

(* Build from parts to omit grouping braces. *)
let entries_of_typcase owner ((typ, _hints) : El.typcase) : entry list =
  match case_head typ with
  | None -> []
  | Some (name, atom, args) ->
      let signature =
        match args with
        | [] -> name
        | args -> name ^ " " ^ El.Print.string_of_typs " " args
      in
      [
        {
          name;
          kind = Case;
          region = atom.at;
          signature = summarise signature;
          detail = Some (Printf.sprintf "Case of `syntax %s`." owner);
          doc = None;
          shape =
            shape (joined " " ([ Literal name ] :: List.map parts_of_typ args));
          fills = Some owner;
          notation = [];
        };
      ]

let entries_of_typfield owner ((atom, plaintyp, _hints) : El.typfield) :
    entry list =
  match atom_name atom with
  | None -> []
  | Some name ->
      [
        {
          name;
          kind = Field;
          region = atom.at;
          signature =
            summarise (name ^ " : " ^ El.Print.string_of_plaintyp plaintyp);
          detail = Some (Printf.sprintf "Field of `syntax %s`." owner);
          doc = None;
          shape = [];
          fills = None;
          notation = [];
        };
      ]

let entries_of_deftyp owner (deftyp : El.deftyp) : entry list =
  match deftyp.it with
  | El.VariantTD typcases -> List.concat_map (entries_of_typcase owner) typcases
  | El.StructTD typfields ->
      List.concat_map (entries_of_typfield owner) typfields
  | El.PlainTD _ -> []

let entries_of_def (def : El.def) : entry list =
  let entry ?(shape = []) ?fills ?(notation = []) name kind (id : El.id)
      signature =
    {
      name;
      kind;
      region = id.at;
      signature;
      detail = None;
      doc = None;
      shape;
      fills;
      notation;
    }
  in
  match def.it with
  | El.SynD ids_tparams ->
      List.map
        (fun ((id : El.id), tparams) ->
          entry id.it Syntax id ~fills:id.it
            ("syntax " ^ with_tparams id.it tparams))
        ids_tparams
  | El.TypD (id, tparams, deftyp, _) ->
      entry id.it Syntax id ~fills:id.it
        (summarise
           ("syntax " ^ with_tparams id.it tparams ^ " = "
           ^ El.Print.string_of_deftyp deftyp))
      :: entries_of_deftyp id.it deftyp
  | El.VarD (id, plaintyp, _) ->
      [
        entry id.it Var id
          ~fills:(El.Print.string_of_plaintyp plaintyp)
          ("var " ^ id.it ^ " : " ^ El.Print.string_of_plaintyp plaintyp);
      ]
  | El.RelD (id, nottyp, _) ->
      [
        (* Relation invocations include their name, colon, and notation. *)
        entry id.it Relation id
          ~shape:(shape (Literal (id.it ^ ": ") :: parts_of_nottyp nottyp))
          ~notation:(parts_of_nottyp nottyp)
          (summarise
             ("relation " ^ id.it ^ ": " ^ El.Print.string_of_nottyp nottyp));
      ]
  | El.RuleD (relid, ruleid, _, _) ->
      let name = relid.it ^ El.Print.string_of_ruleid ruleid in
      [ entry name Rule ruleid ("rule " ^ name) ]
  | El.DecD (id, tparams, params, plaintyp, _)
  | El.BuiltinDecD (id, tparams, params, plaintyp, _) ->
      [
        entry ("$" ^ id.it) Dec id
          ~shape:(shape (dec_shape ("$" ^ id.it) params))
          (summarise
             ("dec $" ^ with_tparams id.it tparams
             ^ El.Print.string_of_params params
             ^ " : "
             ^ El.Print.string_of_plaintyp plaintyp));
      ]
  (* Definitions reuse declarations; separators introduce no symbols. *)
  | El.DefD _ | El.SepD -> []

let of_spec (spec : El.spec) : t = List.concat_map entries_of_def spec

let with_docs ~sources (index : t) : t =
  let docs = Doc.of_sources sources in
  List.map
    (fun entry ->
      {
        entry with
        doc =
          Doc.find docs ~file:entry.region.left.file
            ~line:entry.region.left.line;
      })
    index

let to_list (index : t) = index

(* Resolve metavariables without numeric subscripts or primes. *)
let base_name name =
  let name = String.concat "" (String.split_on_char '\'' name) in
  match String.rindex_opt name '_' with
  | Some i when i > 0 ->
      let suffix = String.sub name (i + 1) (String.length name - i - 1) in
      let numeric =
        suffix <> ""
        && String.for_all
             (fun c -> ('0' <= c && c <= '9') || ('a' <= c && c <= 'z'))
             suffix
      in
      if numeric then String.sub name 0 i else name
  | _ -> name

let find (index : t) name =
  let by_name n = List.filter (fun entry -> String.equal entry.name n) index in
  match by_name name with
  | [] ->
      let base = base_name name in
      if String.equal base name then [] else by_name base
  | found -> found

let in_file (index : t) file =
  List.filter (fun entry -> String.equal entry.region.left.file file) index

let declares (index : t) name =
  List.exists
    (fun entry ->
      match entry.kind with
      | Syntax | Var -> String.equal entry.name name
      | _ -> false)
    index
