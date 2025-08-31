open Util.Source

let strip_var_suffix id =
  let rec is_sub id idx =
    idx = String.length id || (id.[idx] = '_' && is_sub id (idx + 1))
  in
  match (String.index_opt id.it '_', String.index_opt id.it '\'') with
  | None, None -> id
  | Some idx, None when is_sub id.it idx -> id
  | None, Some idx | Some idx, None -> String.sub id.it 0 idx $ id.at
  | Some idx_a, Some idx_b -> String.sub id.it 0 (min idx_a idx_b) $ id.at
