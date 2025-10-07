let nand p q = if (p = q) && (p = true) then false else true;;

let nand2 p q = match p,q with
        true,true -> false
|       _ -> true;;


let () = print_endline "Hello, World!"
