let countzero f a b = 
  let rec tail_countzero acc f a b = 
    if a > b then acc else (match (f a) with
      0 -> tail_countzero (acc+1) f (a+1) b
    | _ -> tail_countzero acc f (a+1) b) in
  tail_countzero 0 f a b;;

let () = 
  assert (countzero (fun x -> x) (-10) 10 = 1);
  assert (countzero (fun x -> x) 1 10 = 0);
  assert (countzero (fun x -> x*x - 1) (-10) 10 = 2);
  assert (countzero (fun x -> (if x<0 then -x else x) - 1) (-10) 10 = 2);;
