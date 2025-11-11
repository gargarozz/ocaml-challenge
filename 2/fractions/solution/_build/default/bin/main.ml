let is_posfrac (a,b) = 
  b != 0 && (a * b) > 0

let compare_posfrac (a, b) (c,d) = 
  if not (is_posfrac (a,b)) || (is_posfrac (c,d)) then 
    failwith "not a positive frac" 
  else
    let (a', b') = if b < 0 then (-a, -b) else (a, b) in
    let (c', d') = if d < 0 then (-c, -d) else (c, d) in
    
    let estr = a' * d' in
    let med = c' * d' in

    compare med estr

let sign_frac (a,b) = 
  if a = 0 then 0 else if a * b > 0 then 1 else -1

let abs_frac (a,b) = (abs a, abs b)

let compare_frac (a,b) (c,d) = 
  if b = 0 || d = 0 then
    failwith("compare_frac: bad division (0)")
  else
    let sign1 = sign_frac (a,b) in
    let sign2 = sign_frac (c,d) in

    match (sign1, sign2) with
      (0,0) -> 0
    | (1,1) -> compare_posfrac (a,b) (c,d)
    | (1,0) | (1,-1) | (0,-1) -> 1
    | (0,1) | (-1,0) | (-1,1) -> -1
    | (-1,-1) -> let res = compare_posfrac (abs_frac (a,b)) (abs_frac (c,d)) in -res

let () = 
  assert (compare_posfrac (1,2) (2,4) == 0);
  assert (compare_posfrac (1,2) (1,3) == 1);
  assert (compare_posfrac (1,2) (2,3) == -1);;
