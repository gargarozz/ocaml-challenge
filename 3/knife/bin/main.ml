let rec knife l n = match l with
  | [] -> ([],[])
  | x::xs ->
      if n <= 0 then ([],l)
      else let (l,r) = knife xs (n-1) in (x::l, r)

let () = 
  assert (knife [1;2;3;4;5;6] 3 = ([1;2;3], [4;5;6]));
  assert (knife ['b';'r';'e';'a';'d'] 3  = (['b';'r';'e'], ['a';'d']));
  assert (knife [] 0 = ([], []));
  assert (knife ["miss"; "me"] 2  = (["miss"; "me"], []));
  assert (knife ["oops"] (-1)  = ([], ["oops"]));;
