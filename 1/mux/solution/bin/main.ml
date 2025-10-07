let mux2 s0 a b = match s0 with
  | true -> a
  | false -> b;;

let mux4 s1 s0 a0 a1 a2 a3 = match s1,s0 with
  | false, false -> a0
  | false, true -> a1
  | true, false -> a2
  | true, true -> a3;;


let () = print_endline "Hello, World!"
