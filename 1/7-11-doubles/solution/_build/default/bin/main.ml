(*
# 7, 11 and doubles

Write a function with type:
```ocaml
seven_eleven_doubles : unit -> bool * int * int
```
which throws two random dices d1,d2, and returns a triple (b,d1,d2) where b is true iff
the sum of the two dices is 7 or 11, or if the two dices are equal.
*)


let seven_eleven_doubles () : (bool * int * int) =
    let d1 = 1 + Random.int 6 in
    let d2 = 1 + Random.int 6 in
    let b = (d1 + d2 = 7) || (d1 + d2 = 11) || (d1 = d2) in (b, d1, d2);;

let () = Random.self_init ();
    let (b,d1,d2) = seven_eleven_doubles () in
    Printf.printf "Dadi: %d e %d,  b: %b\n" d1 d2 b;;
