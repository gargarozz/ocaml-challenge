let rec lang0 w = 
  match w with
    | [] -> true
    | [0] -> false
    | [_] -> true
    | h::t -> lang0 t
;;


