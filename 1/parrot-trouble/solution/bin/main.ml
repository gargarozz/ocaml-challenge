let parrot_trouble talk time = 
        if time < 0 || time > 23 then None else Some(
                if talk then (if time >= 20 || time < 7 then true else false) else false);;


let () = print_endline "Hello, World!"
