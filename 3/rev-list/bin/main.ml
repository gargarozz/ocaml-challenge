let rec rev l = match l with
    [] -> []
  | a::b -> (rev b) @ [a];;

let tail_rev l =
  let rec tail_rev1 l acc =
    match l with
      [] -> acc
    | a::b -> tail_rev1 b (a::acc) in
  tail_rev1 l [] ;;
