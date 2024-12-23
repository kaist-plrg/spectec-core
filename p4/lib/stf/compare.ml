let equals (input : string) (output : string) =
  let to_list s = List.init (String.length s) (String.get s) in
  let input = to_list input in
  let output = to_list output in
  List.length input = List.length output
  && List.fold_left2
       (fun same i o -> same && (o = '*' || i = o))
       true input output
