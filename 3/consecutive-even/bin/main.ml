let consecutive_even l = 
  let rec loop acc max l = match l with
    [] -> if acc > max then acc else max
  | h::t -> 
      let newmax = if acc > max then acc else max in 
      if h mod 2 = 0 then loop (acc+1) newmax t else loop 0 newmax t in
  loop 0 0 l;;

let () = 
  assert(consecutive_even [] = 0);
  assert(consecutive_even [1;2;3;4;5;6] = 1); 
  assert(consecutive_even [1;2;2;3;4;5] = 2);
  assert(consecutive_even [1;2;3;4;2;5] = 2);
  assert(consecutive_even [1;2;2;3;4;2;5] = 2);
  assert(consecutive_even [1;2;2;2;3;4;2;6;5] = 3);;
