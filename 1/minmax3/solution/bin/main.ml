let minmax3 a b c =
  let arr = [a; b; c] in
  let min = List.fold_left (min) 9999 arr in
  let max = List.fold_left (max) 0 arr in
  (min, max);;

let () = print_endline "Hello, World!"
