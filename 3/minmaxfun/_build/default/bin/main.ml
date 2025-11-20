let minmaxfun f a b = 
  if a > b then None else
    let start_val = f a in
    let rec loop i cur_min cur_max =
      if i > b then
        Some (cur_min, cur_max)
      else
        let v = f i in
        let new_min = if v < cur_min then v else cur_min in
        let new_max = if v > cur_max then v else cur_max in
        loop (i + 1) new_min new_max 
    in
    loop (a + 1) start_val start_val;;

let () = 
  assert (minmaxfun (fun x -> x) (-2) 5 = Some (-2,5));
  assert (minmaxfun (fun x -> x) 5 (-2) = None);
  assert (minmaxfun (fun x -> x) 5 5 = Some (5,5));
  assert (minmaxfun (fun x -> x * x) (-2) 5 = Some (0,25));

  let curve x = x |> Float.of_int |> fun x -> x ** 3.0 -. 3.0 *. x in
  let arccos x = x |> Float.of_int |> Float.acos in

  assert (minmaxfun curve (-2) 2 = Some (-2.0,2.0));
  assert (minmaxfun arccos (-1) 1 = Some (0., Float.pi));;
