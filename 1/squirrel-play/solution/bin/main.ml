type season = Spring | Summer | Autumn | Winter;;

let squirrel_play temp season = 
        let upper_limit = match season with Summer -> 35 | _ -> 30 in
        if temp >= 15 && temp <= upper_limit then true else false;;

let () = print_endline "Hello, World!"
