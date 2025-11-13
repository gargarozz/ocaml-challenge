let tris (a,b,c,d) = 
  (a = b && b = c) ||
  (a = b && b = d) ||
  (a = c && c = d) ||
  (b = c && c = d)

let dado10 () = 
  Random.self_init ();
  (Random.int 10)+1;;

let hand = 
  (dado10 (), dado10 (), dado10 (), dado10 ());;
