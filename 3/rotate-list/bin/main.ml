let rec rotate n l = match l with
  | [] -> []
  | x::xs -> if n > 0 then rotate (n-1) (xs @ [x]) else l;;

let () = 
  assert(rotate 0 [5;6;7;8] = [5;6;7;8]);
  assert(rotate 1 [5;6;7;8] = [6;7;8;5]);
  assert(rotate 2 [5;6;7;8] = [7;8;5;6]);
  assert(rotate 3 [5;6;7;8] = [8;5;6;7]);
  assert(rotate 4 [5;6;7;8] = [5;6;7;8]);;
