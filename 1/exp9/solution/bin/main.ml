let square x = x * x;;

let exp9 x = (square (square (square x))) * x;;

let () = print_endline "Hello, World!"
