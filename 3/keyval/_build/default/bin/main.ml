let apply f key = List.fold_left (fun acc (k,v) -> 
  match acc with
    | Some _ -> acc
    | None -> if k = key then Some v else None
  ) None f
;;

let () = 
  let f0 = [(1, 7); (2, 3); (4, 5); (5, 6); (7, 9); (2, 4); (8, 3)] in
  assert(apply f0 4 = Some 5);
  assert(apply f0 6 = None);
  assert(apply f0 2 = Some 3);;
