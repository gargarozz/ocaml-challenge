let hot = 
  let bit = Random.int(2) in
  match bit with
| 1 -> "tail"
| _ -> "head";;

let () = print_endline "Hello, World!"
