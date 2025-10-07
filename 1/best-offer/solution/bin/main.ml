let best_offer (o1:int option) (o2:int option) (o3:int option) = 
  let lst = List.filter_map (fun x -> x) [o1;o2;o3] in
  match lst with
  | [] -> None
  | x::xs -> Some(List.fold_left max x xs);;


let () = print_endline "Hello, World!"
