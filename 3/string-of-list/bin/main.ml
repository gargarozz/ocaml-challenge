let string_of_list l = 
  let rec tail_sol acc l = match l with
    | [] -> acc^"]"
    | [a] -> acc ^ (string_of_int a)^"]"
    | a::l' -> tail_sol (acc^(string_of_int a)^";") l' in
  match l with
  | [] -> "[]"
  | _ -> tail_sol "[" l
