let rec mem a = function 
  | [] -> false
  | h::t -> if a=h then true else mem a t
;;

let rec subseteq l1 l2 = match l1 with
  | [] -> true
  | h1::t1 -> if mem h1 l2 then subseteq t1 l2 else false
;;

let seteq l1 l2 = 
  let l1 = List.sort compare l1 in
  let l2 = List.sort compare l2 in
  l1 = l2
;;

let rec dup = function
  | [] -> false
  | h::t -> if mem h t then true else dup t
;;

let rec mkset = function
  | [] -> []
  | h::t -> if mem h t then mkset t else h::(mkset t)
;;

let union l1 l2 = mkset (l1 @ l2);;

let rec inter l1 l2 = match l1 with 
  | [] -> []
  | h::t -> if mem h l2 then h::(inter t l2) else inter t l2
;;

let rec diff l1 l2 = match l1 with
  | [] -> []
  | h::t -> if mem h l2 then diff t l2 else h::(diff t l2)
;;

let dsum xl yl = 
  let rec tag label l = match l with
    | [] -> []
    | h::t -> (label,h)::(tag label t)
  in (tag 0 xl) @ (tag 1 yl)
;;

let rec powset l = match l with
  | [] -> [ [] ]
  | h::t -> 
      let no_h = powset t in
      let with_h = List.map (fun subs -> h::subs) no_h in
      no_h @ with_h
;;
