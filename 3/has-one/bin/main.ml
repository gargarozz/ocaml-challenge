let rec has_one n = if n < 0 then failwith "negative" else 
  if (n mod 10) = 1 then true else
  if n < 10 then false else
  has_one (n/10)

let () = print_endline "Hello, World!"
