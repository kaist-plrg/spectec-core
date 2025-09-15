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

(* Information about the source location *)

type info =
  | M of string
  | I of {
      filename : string;
      line_start : int;
      line_end : int option;
      col_start : int;
      col_end : int;
    }

let pp fmt = function
  | M s -> Format.pp_print_string fmt s
  | I { filename; line_start; line_end; col_start; col_end } -> (
      let f = "File " ^ filename ^ "," in
      match line_end with
      | None ->
          Format.fprintf fmt "%s line %d, characters %d-%d" f line_start
            col_start col_end
      | Some line_end ->
          Format.fprintf fmt
            "%s from line %d character %d to line %d character %d" f line_start
            col_start line_end col_end)

let no_info = M ""
let is_no_info = function M "" -> true | _ -> false

let start_pos = function
  | M _ -> (Int.max_int, Int.max_int)
  | I { line_start; col_start; _ } -> (line_start, col_start)

let end_pos = function
  | M _ -> (0, 0)
  | I { line_start; line_end; col_end; _ } -> (
      match line_end with
      | None -> (line_start, col_end)
      | Some line_end -> (line_end, col_end))

let min_pos (l1, c1) (l2, c2) =
  if l1 < l2 then (l1, c1)
  else if l1 > l2 then (l2, c2)
  else if c1 < c2 then (l1, c1)
  else (l2, c2)

let max_pos (l1, c1) (l2, c2) =
  if l1 > l2 then (l1, c1)
  else if l1 < l2 then (l2, c2)
  else if c1 > c2 then (l1, c1)
  else (l2, c2)

let follows i1 i2 =
  match (i1, i2) with
  | M _, _ | _, M _ -> false
  | I _, I _ ->
      let l1, c1 = end_pos i1 in
      let l2, c2 = start_pos i2 in
      l1 = l2 && c1 = c2

let merge i1 i2 =
  match (i1, i2) with
  | M _, _ -> i2
  | _, M _ -> i1
  | I { filename = f1; _ }, I { filename = f2; _ } when f1 = f2 ->
      let line_start, col_start = min_pos (start_pos i1) (start_pos i2) in
      let line_end, col_end = max_pos (end_pos i1) (end_pos i2) in
      let line_end = if line_start = line_end then None else Some line_end in
      I { filename = f1; line_start; line_end; col_start; col_end }
  | _ ->
      Format.eprintf "Cannot merge info from different files: %a and %a@" pp i1
        pp i2;
      assert false

(* Phrase *)

type ('a, 'b) note_phrase = { it : 'a; at : info; note : 'b }
type 'a phrase = ('a, unit) note_phrase

let ( $ ) it at = { it; at; note = () }
let ( $$ ) it (at, note) = { it; at; note }
let ( % ) at note = (at, note)
let it { it; _ } = it
let at { at; _ } = at
let note { note; _ } = note

let to_region (info : info) : Util.Source.region =
  match info with
  | M _ -> Util.Source.no_region
  | I { filename; line_start; line_end; col_start; col_end } ->
      let left =
        { Util.Source.file = filename; line = line_start; column = col_start }
      in
      let right =
        match line_end with
        | None -> { left with column = col_end }
        | Some line_end -> { left with line = line_end; column = col_end }
      in
      { Util.Source.left; right }
