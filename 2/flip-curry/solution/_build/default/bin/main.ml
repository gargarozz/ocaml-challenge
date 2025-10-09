let flip f a b = (f b a);;

let () = let sub x y = x - y in
let flipped_sub = flip sub in
assert (flipped_sub 3 10 = 7);
assert (flipped_sub 10 3 = -7);;
