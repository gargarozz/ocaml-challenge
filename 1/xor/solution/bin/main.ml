let xor (p:bool) (q:bool) = if p != q then true else false;;

let xor2 (p:bool) (q:bool) = match p,q with
        true,true | false, false -> true
        | _ -> false;;

let () = print_endline "Hello, World!"
