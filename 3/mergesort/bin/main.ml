let rec knife l n = match l with
  | [] -> ([],[])
  | x::xs ->
      if n <= 0 then ([],l)
      else let (l,r) = knife xs (n-1) in (x::l, r)
;;

let merge l1 l2 =
  let rec helper acc l1 l2 = 
    match l1,l2 with
      | [],[] -> acc
      | [],a -> acc@a
      | a,[] -> acc@a
      | x::xs,y::ys -> if x<y then helper (acc@[x]) xs l2 else helper (acc@[y]) l1 ys
  in helper [] l1 l2
;;

let halve l = knife l ((List.length l)/2);;

let rec merge_sort l =
  match l with
    | [] -> []
    | [x] -> [x]
    | ls -> let (l1,l2) = halve ls in merge (merge_sort l1) (merge_sort l2)
;;

let () =
  assert (merge [1;4;5] [2;3;6] = [1;2;3;4;5;6]);
  assert (merge [7] [2;3;6] = [2;3;6;7]);
  assert (merge [7] [] = [7]);
  assert (halve [5;1;3;8;-2;6] = ([5;1;3], [8;-2;6]));
  assert (halve [1;3] = ([1], [3]));
  assert (halve [5;1;3] = ([5], [1;3]));
  assert (merge_sort [5;1;3;8;-2;6] = [-2;1;3;5;6;8]);;
