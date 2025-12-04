(* per definire una funzione ricorsiva si usa let rec *)
let rec sum n =
  if n = 0 then
    0
  else n+(sum (n-1))
;;

(* la concatenazione si fa con l'operatore @ *)
let concatenazione = prima @ seconda;;

(* ocaml vede le liste come una catena di append che termina con []. *)
let rec somma l = match l with
  [] -> 0
  | a::tl -> a+(somma tl)
;;
