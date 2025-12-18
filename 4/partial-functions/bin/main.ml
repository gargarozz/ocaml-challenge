let mkset l = List.fold_left (fun acc x -> if mem x acc then acc else x::acc) [] l;;

let subseteq xl yl = if xl = [] then true else List.fold_left (fun acc x -> if acc then mem x yl else false) true xl;;

let seteq l1 l2 =
  (subseteq l1 l2) && ((List.length l1) = (List.length l2))
;;

let is_fun _A _B f = let (A',B') = List.fold_left (fun (acc_a, acc_b) (a,b) -> (a::acc_a,b::acc_b)) ([],[]) f in
  (subseteq (mkset A') _A) && (subseteq (mkset B') _B)
;;

let is_tot _A _B f = let (A',B') = List.fold_left (fun (acc_a, acc_b) (a,b) -> (a::acc_a,b::acc_b)) ([],[]) f in
  (seteq (mkset A') _A) && (subseteq (mkset B') _B)
;;

