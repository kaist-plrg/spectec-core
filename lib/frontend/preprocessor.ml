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

open Core

let preprocess includes filename =
  let cmd =
    String.concat ~sep:" "
      ([ "cc" ]
      @ List.map ~f:(Printf.sprintf "-I%s") includes
      @ [ "-undef"; "-nostdinc"; "-E"; "-x"; "c"; filename ])
  in
  let in_chan = Core_unix.open_process_in cmd in
  (*
    let devnull = Core_unix.openfile "/dev/null" ~mode:[O_WRONLY] in
    let _ = Core_unix.dup2 ~src:devnull ~dst:Core_unix.stderr () in
    let _ = Core_unix.close devnull in
  *)
  let program = In_channel.input_all in_chan in
  let _ = Core_unix.close_process_in in_chan in
  program
