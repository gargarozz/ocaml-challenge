type point = {x : float; y : float};;
type color = Red | Green | Blue;;
type colored_point = point * color;;

let translate p dx dy = {x=p.x+.dx; y=p.y+.dy};;

let distance_from_origin p = Float.sqrt ((p.x *. p.x) +. (p.y *. p.y));;

let same_color cp1 cp2 = 
  let (_, c1) = cp1 in
  let (_, c2) = cp2 in c1 = c2;;

let recolor cp new_c = let (p,_) = cp in (p,new_c);;


let () = 
  (* Creiamo alcuni punti di prova *)
  let p1 = { x = 0.0; y = 0.0 } in
  let p2 = { x = 3.0; y = 4.0 } in

  (* Test translate *)
  assert (translate p1 1.0 1.0 = { x = 1.0; y = 1.0 });
  assert (translate p2 (-3.0) (-4.0) = p1);

  (* Test distance_from_origin *)
  assert (distance_from_origin p1 = 0.0);
  (* Triangolo 3-4-5: la distanza deve essere 5.0 *)
  assert (distance_from_origin p2 = 5.0); 

  (* Creiamo punti colorati *)
  let cp_red = (p1, Red) in 
  let cp_blue = (p2, Blue) in
  let cp_red_2 = (p2, Red) in

  (* Test same_color *)
  assert (same_color cp_red cp_red_2 = true);  (* Entrambi Red *)
  assert (same_color cp_red cp_blue = false); (* Red vs Blue *)

  (* Test recolor *)
  (* Cambiamo il colore di cp_blue in Red. Il risultato deve essere uguale a cp_red_2? 
     Sì, perché cp_red_2 è (p2, Red) e noi stiamo trasformando (p2, Blue) in (p2, Red) *)
  assert (recolor cp_blue Red = cp_red_2);

  print_endline "Tutti i test passati con successo!";;
