let both_true (p:('a -> bool)) (q:('a -> bool)) (a:'a) = (p a) = (q a);;

let () = let is_positive x = x > 0 in
let is_even x = x mod 2 = 0 in
let is_positive_and_even = both_true is_positive is_even in
assert(is_positive_and_even 4 = true);
assert(is_positive_and_even 3 = false);;
