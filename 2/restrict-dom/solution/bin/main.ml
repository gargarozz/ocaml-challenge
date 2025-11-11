let restrict f g x = if g x then Some(f x) else None;;

let () = 
  let f1 = restrict succ (fun x -> x>0) in
  let f2 = restrict (fun (x,y) -> x - y) (fun (x,y) -> x-y>=0) in

  assert (f1 1 = Some 2) ;
  assert (f1 0 = None) ;
  assert(f2 (5,2) = Some 3);
  assert(f2 (5,6) = None);;
