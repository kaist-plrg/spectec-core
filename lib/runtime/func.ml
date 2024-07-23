open Syntax.Ast
open Util.Source

(* Runtime representation of functions *)

type t =
  | FuncF of {
      vis_glob : Vis.vis;
      tparams : tparam list;
      params : param list;
      ret : Type.t;
      body : block;
    }
  (* (TODO) Consider return type, which may be a type variable *)
  | ExternF of {
      vis_glob : Vis.vis;
      tparams : tparam list;
      params : param list; (* ret : Type.t; *)
    }
  | MethodF of {
      vis_obj : Vis.vis;
      tparams : tparam list;
      params : param list;
      body : block;
    }
  | ExternMethodF of {
      vis_obj : Vis.vis;
      tparams : tparam list;
      params : param list; (* ret : Type.t; *)
    }
  | StateF of { body : block }
  (* The visibility of an action depends on its declaration position *)
  | ActionF of { vis : Vis.vis; params : param list; body : block }
  | TableF of { vis_obj : Vis.vis }

let pp fmt = function
  | FuncF _ -> Format.fprintf fmt "function"
  | ExternF _ -> Format.fprintf fmt "extern"
  | MethodF _ -> Format.fprintf fmt "method"
  | ExternMethodF _ -> Format.fprintf fmt "extern"
  | StateF _ -> Format.fprintf fmt "state"
  | ActionF _ -> Format.fprintf fmt "action"
  | TableF _ -> Format.fprintf fmt "table"

(* Getters *)

let get_params = function
  | FuncF { params; _ }
  | ExternF { params; _ }
  | MethodF { params; _ }
  | ExternMethodF { params; _ }
  | ActionF { params; _ } ->
      List.map (fun { it = id, _, _, _; _ } -> id.it) params
  | _ -> []
