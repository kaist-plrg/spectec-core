(* Helper for random sampling *)

let random_select (lst : 'a list) : 'a option =
  if List.length lst = 0 then None
  else Random.int (List.length lst) |> List.nth lst |> Option.some

let random_sample (size : int) (lst : 'a list) : 'a list =
  let arr = Array.of_list lst in
  let len = Array.length arr in
  if len <= size then lst
  else
    let idxs = Hashtbl.create size in
    let rec random_sample' lst_sample =
      if List.length lst_sample = size then lst_sample
      else
        let idx = Random.int len in
        if Hashtbl.mem idxs idx then random_sample' lst_sample
        else (
          Hashtbl.add idxs idx ();
          random_sample' (arr.(idx) :: lst_sample))
    in
    random_sample' []

let random_sample_weighted (size : int) (probs : float list) (lst : 'a list) :
    'a list =
  List.combine probs lst
  |> List.filter (fun (p, _) -> Random.float 1.0 < p)
  |> List.map snd |> random_sample size

(* Shuffling a list *)

let shuffle (lst : 'a list) : 'a list =
  let rec shuffle' (lst_shuffled : 'a list) (lst : 'a list) =
    match lst with
    | [] -> lst_shuffled
    | _ ->
        let idx_pick = Random.int (List.length lst) in
        let item = List.nth lst idx_pick in
        let lst = List.filteri (fun idx _ -> idx <> idx_pick) lst in
        shuffle' (item :: lst_shuffled) lst
  in
  shuffle' [] lst
