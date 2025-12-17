let mem x s = List.fold_left (fun acc a -> if acc then acc else a=x) false s;;

assert(mem 1 [1;3;5]);;
assert(mem 2 [1;3;5] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;
assert(mem [1;2] [[1];[2];[1;2]]);;

let subseteq xl yl = if xl = [] then true else List.fold_left (fun acc x -> if acc then mem x yl else false) true xl;;

assert(subseteq [] [1;3;5]);;
assert(subseteq [1;5] [5;1]);;
assert(subseteq [1;5] [1;3;5]);;
assert(subseteq [1;5] [5;3;1]);;
assert(subseteq [2] [1;3;5] = false);;
assert(subseteq [[1;2]] [[1];[2];[2;1]] = false);;
assert(subseteq [[1];[2;1]] [[1];[2];[2;1]]);;

let seteq l1 l2 =
  (subseteq l1 l2) && ((List.length l1) = (List.length l2))
;;

assert(seteq [1;5;3] [1;3;5]);;
assert(seteq [1;5;2] [1;3;5] = false);;
assert(seteq [[1;2]] [[2;1]] = false);;
assert(seteq [[1];[1;2]] [[1;2];[1]]);;
assert(mem [1;2] [[1];[2];[2;1]] = false);;

let dup l = let (l',result) = 
  List.fold_left (fun (list, found) x -> 
    if found then (x::list, true) else (x::list, (mem x list))) ([],false) l 
  in result
;;

assert(dup [] = false);;
assert(dup [1;1]);;
assert(dup [1;3;5] = false);;
assert(dup [1;3;5;3]);;

let mkset l = List.fold_left (fun acc x -> if mem x acc then acc else x::acc) [] l;;

assert(seteq (mkset [1;2;3;2;1]) [1;2;3]);;
assert(seteq (mkset [1;2;1;2;1]) [1;2]);;
assert(seteq (mkset [1;2;3]) [2;3;1]);;

let union l1 l2 = mkset (l1 @ l2);;

let inter l1 l2 = List.fold_left (fun acc x -> if mem x l2 then x::acc else acc) [] l1;;

let diff l1 l2 = List.fold_left (fun acc x -> if mem x l2 then acc else x::acc) [] l1;;

assert(seteq (union [1;2;3] []) [1;2;3]);;
assert(seteq (union [] [2;3;4]) [2;3;4]);;
assert(seteq (union [1;2;3] [2;3;4]) [1;2;3;4]);;

assert(seteq (inter [1;2;3] []) []);;
assert(seteq (inter [] [2;3;4]) []);;
assert(seteq (inter [1;2;3] [2;3;4]) [2;3]);;

assert(seteq (diff [1;2;3] []) [1;2;3]);;
assert(seteq (diff [] [2;3;4]) []);;
assert(seteq (diff [1;2;3] [2;3;4]) [1]);;
assert(seteq (diff [1;2;3] [3;1]) [2]);;

let dsum l1 l2 = (List.fold_left (fun acc x -> (0,x)::acc) [] l1) @ 
  (List.fold_left (fun acc x -> (1,x)::acc) [] l1)
;;

assert(seteq (dsum [1;2;3] []) [(0,1);(0,2);(0,3)]);;
assert(seteq (dsum [] [2;3;4]) [(1,2);(1,3);(1,4)]);;
assert(seteq (dsum [1;2] [2;3]) [(0,1);(0,2);(1,2);(1,3)]);;

(*
  1. prendo il primo elemento della lista
  2. calcolo il suo power set
  3. prendo il secondo elemento e lo unisco agli elementi del power set del precedente
  4. unisco il power set appena ottenuto con quello del primo non toccato
  5. cosi via
*)

let powset l = List.fold_left (fun acc x -> acc @ List.map (fun a -> x::a) acc) [[]] l;;

assert (powset [] = [[]]);;
assert (seteq (powset [1]) [[];[1]]);;
assert (List.length (powset [1;2]) = 4);;
assert (List.length (powset [1;2;3]) = 8);;
assert (List.length (powset [1;2;3;4]) = 16);;
