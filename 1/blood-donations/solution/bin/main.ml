type blood_group = A | B | AB | O;;

let check_groups p1 p2 = match p1,p2 with
  | _,AB -> true
  | O,_ -> true
  | A,A | B,B -> true
  | _ -> false;;

let () = print_endline "Hello, World!"
