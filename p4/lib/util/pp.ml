module F = Format

let indent level = String.make (2 * level) ' '

let pp_list pp_elem sep fmt l =
  F.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt sep) pp_elem)
    l

let pp_option pp_elem fmt = function Some x -> pp_elem fmt x | None -> ()

let pp_pairs ?(trailing = false) ?(level = 0) pp_k pp_v rel sep fmt pairs =
  F.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> F.fprintf fmt sep)
       (fun fmt (k, v) ->
         F.fprintf fmt "%s%a%s%a" (indent level) pp_k k rel pp_v v))
    pairs;
  if trailing && pairs <> [] then F.fprintf fmt sep
