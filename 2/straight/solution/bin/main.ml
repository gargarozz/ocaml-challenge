type suit = S | H | D | C;;
type card = Card of int * suit;;

Random.self_init ();;

let rndCard () = 
  let rndSuit = match Random.int 4 with
    0 -> S
  | 1 -> H
  | 2 -> D
  | 3 -> C in
  Card((Random.int 13)+1, rndSuit)

let rndHand () = 
  (rndCard(), rndCard(), rndCard(), rndCard(), rndCard());;

let get_rank (Card(r, _ )) = r;;

let straight (c1,c2,c3,c4,c5) = 
  let r1 = get_rank c1 in
  let r2 = get_rank c2 in
  let r3 = get_rank c3 in
  let r4 = get_rank c4 in
  let r5 = get_rank c5 in

  (* inserisci i valori in una lista e ordinala *)
  let ranks_list = [r1;r2;r3;r4;r5] in
  let sorted_ranks = List.sort compare ranks_list in

  (* estrai i valori ordinati e controlla se 4 sono uguali *)
  match sorted_ranks with
  | [a;b;c;d;e] -> (a = (b-1) && b = (c-1) && c = (d-1) && d = (e-1))
  | _ -> false;;
