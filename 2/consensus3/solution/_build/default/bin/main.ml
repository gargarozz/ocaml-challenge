let consensus3 (f1, f2, f3) n = 
  let r1 = f1 n in
  let r2 = f2 n in
  let r3 = f3 n in
  if r1 = r2 then Some r1 else
  if r1 = r3 then Some r1 else
  if r2 = r3 then Some r2 else
  None;;

let () = 
  assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 1 = Some 5);
  assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 2 = Some 2);
  assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 3 = None);
  assert (
    try
      consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 0
      |> ignore; false
    with _ -> true);;
