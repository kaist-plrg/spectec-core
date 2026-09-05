(** Retain elaborated types across edits that temporarily fail. *)

module Il = Lang.Il

type t = {
  aliases : (string, string) Hashtbl.t;
      (** Map each alias to its immediate target. *)
  relations : (string, string list) Hashtbl.t;
      (** Map relations to argument types in notation order. *)
}

let empty = { aliases = Hashtbl.create 1; relations = Hashtbl.create 1 }

(* Omit parameterised aliases: targets depend on use-site arguments. *)
let of_il (spec : Il.spec option) =
  match spec with
  | None -> empty
  | Some defs ->
      let t = { aliases = Hashtbl.create 64; relations = Hashtbl.create 64 } in
      List.iter
        (fun (def : Il.def) ->
          match def.it with
          | Il.TypD { synid; tparams = []; deftyp } -> (
              match deftyp.it with
              | Il.PlainT typ ->
                  Hashtbl.replace t.aliases synid.it
                    (Il.Print.string_of_typ typ)
              | _ -> ())
          | Il.RelD { relid; reltyp; _ } ->
              let types =
                Il.Mixfix.args reltyp.it
                |> List.map (function Il.Mode.In typ | Il.Mode.Out typ ->
                       Il.Print.string_of_typ typ)
              in
              Hashtbl.replace t.relations relid.it types
          | _ -> ())
        defs;
      t

(* Bound alias traversal to tolerate temporarily cyclic definitions. *)
let canonical t name =
  let rec go fuel name =
    if fuel <= 0 then name
    else
      match Hashtbl.find_opt t.aliases name with
      | Some next when not (String.equal next name) -> go (fuel - 1) next
      | _ -> name
  in
  go 8 (String.trim name)

let hole_type t ~relation ~index =
  match Hashtbl.find_opt t.relations relation with
  | None -> None
  | Some types -> List.nth_opt types index
