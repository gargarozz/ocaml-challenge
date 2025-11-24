let rec is_perforated l = 
  match l with
  | [] -> true
  | [_] -> true
  | x1::x2::tail -> if abs (x1-x2) < 2 then false
      else is_perforated (x2::tail)

let () = 
  assert(is_perforated []);
  assert(is_perforated [1]);
  assert(is_perforated [1;2] = false);
  assert(is_perforated [1;3]);
  assert(is_perforated [1;5;2]);
  assert(is_perforated [1;3;2] = false);
  assert(is_perforated [1;4;2;0]);
  assert(is_perforated [1;3;2;0] = false);
  assert(is_perforated [1;3;5;2;4;7;3;1]);;
