(* Copyright 2018-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 *)

module SMap = Map.Make (String)

type has_params = bool
type ident_kind = TypeName of has_params | Ident of has_params
type t = ident_kind SMap.t list

(* Current context, stored as a mutable global variable *)
let context : t ref = ref [ SMap.empty ]
let backup : t ref = ref []

(* Resets context *)
let reset () =
  context := [ SMap.empty ];
  backup := []

(* Associates [id] with [k] in map for current scope *)
let declare (id : string) (k : ident_kind) : unit =
  match !context with
  | [] -> failwith "ill-formed context"
  | m :: l ->
      Debug_config.context_debug_print ">>> Declaring %s as %s\n" id
        (match k with TypeName _ -> "TypeName" | Ident _ -> "Ident");
      context := SMap.add id k m :: l

let declare_type id has_params = declare id (TypeName has_params)
let declare_types types = List.iter (fun s -> declare_type s false) types
let declare_var id has_params = declare id (Ident has_params)
let declare_vars vars = List.iter (fun s -> declare_var s false) vars

(* Tests whether [id] is known as a type name. *)
let get_kind (id : string) : ident_kind =
  let rec loop = function
    | [] -> Ident false
    | m :: rest -> (
        match SMap.find_opt id m with None -> loop rest | Some k -> k)
  in
  loop !context

let is_typename (id : string) : bool =
  match get_kind id with TypeName _ -> true | _ -> false

let mark_template (id : string) =
  let rec loop = function
    | [] -> []
    | m :: rest -> (
        match SMap.find_opt id m with
        | None -> m :: loop rest
        | Some (TypeName _) -> SMap.add id (TypeName true) m :: rest
        | Some (Ident _) -> SMap.add id (Ident true) m :: rest)
  in
  context := loop !context

(* Takes a snapshot of the current context. *)
let push_scope () =
  Debug_config.context_debug_print "[[ Pushing scope\n";
  context := SMap.empty :: !context

(* Remove scope *)
let pop_scope () =
  Debug_config.context_debug_print "]] Popping scope\n";
  match !context with
  | [] -> failwith "ill-formed context"
  | [ _ ] -> failwith "pop would produce ill-formed context"
  | _ :: l -> context := l

let go_toplevel () =
  let rec loop c =
    match c with
    | [] -> failwith "ill-formed context"
    | [ _ ] -> context := c
    | _ :: l -> loop l
  in
  backup := !context;
  loop !context

let go_local () = context := !backup

(* Printing functions for debugging *)
let print_entry x k =
  match k with
  | TypeName true -> Printf.printf "%s : type<...>" x
  | TypeName false -> Printf.printf "%s : type" x
  | Ident true -> Printf.printf "%s : ident<...>" x
  | Ident false -> Printf.printf "%s : ident" x

let print_map m =
  SMap.iter
    (fun x k ->
      print_entry x k;
      print_endline "")
    m

let print_context () =
  List.iter
    (fun m ->
      print_map m;
      print_endline "----")
    !context
