let correct a = if a < 1 || a > 5 then false else true;;

let is_even x = (x mod 2) = 0;; 

let win a b = match correct a, correct b with
        false, false -> 0
        | true, false -> 1
        | false, true -> -1
        | _ -> match is_even (a+b) with
                true -> 1
                | false -> -1;;


let () = print_endline "Hello, World!"
