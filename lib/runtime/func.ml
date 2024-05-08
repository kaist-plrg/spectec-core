open Surface.Ast
open Scope

type t =
  | FNormal of {
      name : string;
      params : Parameter.t list;
      genv : Env.t;
      lenv : Env.t;
      body : Statement.t list;
    }
  | FAction of {
      name : string;
      params : Parameter.t list;
      genv : Env.t;
      lenv : Env.t;
      body : Statement.t list;
    }
  (* (TODO) Consider refactoring this into FNormal,
     by merging Parser.transition into Statement.t *)
  | FParser of {
      name : string;
      params : Parameter.t list;
      genv : Env.t;
      lenv : Env.t;
      body : Statement.t list;
      transition : Parser.transition;
    }
  | FExtern of {
      name : string;
      tparams : string list;
      params : Parameter.t list;
    }

let pp fmt func =
  let depth = 4 in
  let indent depth = String.make (depth * 2) ' ' in
  match func with
  | FNormal { name; genv; lenv; _ }
  | FAction { name; genv; lenv; _ }
  | FParser { name; genv; lenv; _ } ->
      Format.fprintf fmt "%sFunction %s {\n%sgenv = %s\n%slenv = %s }"
        (indent depth) name
        (indent (depth + 2))
        (Format.asprintf "%a" Env.pp genv)
        (indent (depth + 2))
        (Format.asprintf "%a" Env.pp lenv)
  | FExtern { name; _ } ->
      Format.fprintf fmt "%sFunction %s { extern }" (indent depth) name
