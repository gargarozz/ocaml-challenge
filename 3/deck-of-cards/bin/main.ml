type suit = Spades | Hearts | Diamonds | Clubs;;
type card = Card of int * suit;;

let complete_deck = 
  List.init 10 (fun a -> Card(a+1, Spades)) @
  List.init 10 (fun a -> Card(a+1, Hearts)) @
  List.init 10 (fun a -> Card(a+1, Diamonds)) @
  List.init 10 (fun a -> Card(a+1, Clubs))
;;

let compare_cards (Card(n1,s1)) (Card(n2,s2)) =
  let suit_comp = compare s1 s2 in
  if suit_comp <> 0 then 
    suit_comp
  else 
    compare n1 n2
;;

let is_complete l = 
  let ordered_l = List.sort compare_cards l in
  ordered_l = complete_deck
;;


(*
  inefficiente per grandi liste, perche' l'operatore di concatenazione ha complessita O(n2) molto brutto 
  ha piu senso mettere elementi in testa e poi invertire la lista alla fine
*)
let extract i l =
  let rec helper acc i l =
    match l with
      | [] -> raise (Failure "index out of bounds")
      | x::xs -> if i = 0 then (x, (acc@xs)) else helper (acc@[x]) (i-1) xs
  in helper [] i l
;;

let gen_deck () =
  let deck = complete_deck in
  let rec loop acc max_i d = 
    match d with
      | [] -> acc
      | a -> let (x,l) = extract (Random.int max_i) d in loop (x::acc) (max_i-1) l
  in loop [] 40 deck
;;
