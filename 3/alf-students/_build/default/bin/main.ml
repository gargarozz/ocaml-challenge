type student = {
  id: string;
  name: string;
  surname: string;
  vote: int option;
  laude: bool
}

let alf2023 = [
  { id="60/61/65570"; name="Ambra"; surname="Ambu"; vote=Some 21; laude=false };
  { id="61/61/65778"; name="Brunello"; surname="Brundu"; vote=Some 18; laude=false };
  { id="60/61/65624"; name="Costantino"; surname="Cossu"; vote=Some 24; laude=false };
  { id="60/61/65808"; name="Deborah"; surname="Demurtas"; vote=Some 28; laude=false };
  { id="60/61/65668"; name="Efisio"; surname="Ennas"; vote=Some 18; laude=false };
  { id="60/61/65564"; name="Felicino"; surname="Frau"; vote=None; laude=false };
  { id="60/64/20203"; name="Gavino"; surname="Girau"; vote=Some 20; laude=false };
  { id="60/61/65892"; name="heidi"; surname="hernandez"; vote=Some 8; laude=true };
  { id="60/61/65563"; name="Igino igor"; surname="Ibba"; vote=Some 15; laude=false };
  { id="60/61/64427"; name="Lillo"; surname="Lilliu"; vote=Some 25; laude=false };
  { id="60/61/65448"; name="Morgan"; surname="Murtas"; vote=Some 15; laude=false };
  { id="61/61/65213"; name="Nathan"; surname="Nieddu"; vote=Some 16; laude=false };
  { id="60/61/65832"; name="Ornella"; surname="Onnis"; vote=Some 30; laude=true };
  { id="60/61/65517"; name="Pinuccio"; surname="Puddu"; vote=Some 28; laude=false };
  { id="60/64/21222"; name="Quintilio"; surname="Quaglioni"; vote=Some 22; laude=false };
  { id="60/61/65907"; name="Rihanna"; surname="Ruzzu"; vote=Some 18; laude=false };
  { id="60/61/65766"; name="Samantah"; surname="Sulis"; vote=Some 30; laude=false };
  { id="60/61/65730"; name="Tatiana"; surname="Truzzu"; vote=Some 30; laude=true };
  { id="60/61/65738"; name="Ubaldo"; surname="Urru"; vote=None; laude=true };
  { id="60/61/65722"; name="Valentina"; surname="Vargiu"; vote=Some 30; laude=true };
  { id="60/61/65592"; name="Zlatan"; surname="Zuncheddu"; vote=Some 18; laude = false }
];;

let id_of_noshow l = 
  let rec loop acc l = match l with
    | [] -> List.map (fun a -> a.id) acc
    | x::xs -> match x.vote with
                | Some _ -> loop acc xs
                | None -> loop (x::acc) xs
  in loop [] l
;;

let passed l =
  let rec loop acc l = match l with
    | [] -> acc
    | x::xs -> match x.vote with None -> loop acc xs | Some v -> if v >= 18 then 
        loop (x::acc) xs
      else loop acc xs
  in loop [] l
;;

let upgradeable l =
  let rec loop acc l = match l with
    | [] -> List.map (fun a -> a.name^" "^a.surname) acc
    | x::xs -> match x.vote with None -> loop acc xs |
      Some v -> if v >= 15 && v <= 17 
        then 
          loop (x::acc) xs 
        else 
          loop acc xs
  in loop [] l
;;

let upgrade l =
  let rec loop acc l = match l with
    | [] -> acc
    | x::xs -> match x.vote with None -> loop acc xs |
      Some v -> if v >= 15 && v <= 17 
        then
          let newx = {id=x.id; name=x.name; surname=x.surname; vote=Some 18; laude=false} in
          loop (newx::acc) xs 
        else 
          loop (x::acc) xs
  in loop [] l
;;

let wrong_laude l =
  let rec loop acc l = match l with
    | [] -> List.map (fun a -> a.name^" "^a.surname) acc
    | x::xs -> match x.vote with None -> loop (x::acc) xs |
      Some v -> if v < 30 && x.laude
        then 
          loop (x::acc) xs
        else 
          loop acc xs
  in loop [] l
;;

let rec fix_laude l = match l with
  | [] -> []
  | x::xs -> match x.vote with None -> x::fix_laude xs | Some v -> 
    if
      v < 30 && x.laude 
    then
      {id=x.id; name=x.name; surname=x.surname; vote=x.vote; laude=false}::(fix_laude xs)
    else 
      x::(fix_laude xs)
;;


let percent_passed l = 
  let tot = List.length l in
  let pass = List.length (passed l) in
  (pass / tot) * 100
;;

let avg_vote l = 
  let pass = passed l in
  let sum = List.fold_left (fun a b -> 
    let v2 = match b.vote with 
      | None -> 0 
      | Some n -> if b.laude then n+2 else n 
    in a + v2
  ) 0 pass in
  let n_pass = List.length pass in

  float_of_int sum /. float_of_int n_pass
;;

(* Funzione di aiuto per i test: ordina una lista generica *)
let sorted l = List.sort compare l;;

(* --- TEST ID_OF_NOSHOW --- *)
(* Ordiniamo entrambi i lati dell'assert *)
assert (sorted (id_of_noshow alf2023) = sorted ["60/61/65564"; "60/61/65738"]);;

(* --- TEST UPGRADEABLE --- *)
assert (sorted (upgradeable alf2023) = sorted ["Igino igor Ibba"; "Morgan Murtas"; "Nathan Nieddu"]);;

(* --- TEST UPGRADE --- *)
(* Qui controlliamo se la lista risultante è vuota. [] ordinato è sempre [], 
   quindi questo test andava già bene, ma lo lasciamo così. *)
assert (upgradeable (upgrade alf2023) = []);;

(* Controllo puntuale: List.find cerca scorrendo, quindi l'ordine non importa *)
let upgraded_list = upgrade alf2023 in
let igino = List.find (fun s -> s.surname = "Ibba") upgraded_list in
assert (igino.vote = Some 18);;

(* --- TEST WRONG_LAUDE --- *)
assert (sorted (wrong_laude alf2023) = sorted ["heidi hernandez"; "Ubaldo Urru"]);;

(* --- TEST FIX_LAUDE --- *)
assert (wrong_laude (fix_laude alf2023) = []);;

(* Controllo puntuale: anche qui l'ordine non conta *)
let fixed_list = fix_laude alf2023 in
let heidi = List.find (fun s -> s.name = "heidi") fixed_list in
assert (heidi.laude = false);;

(* --- TEST PERCENT_PASSED --- *)
(* Restituisce un int, l'ordine non c'entra *)
assert (percent_passed alf2023 = 71);;

(* --- TEST AVG_VOTE --- *)
(* Restituisce un float, l'ordine non c'entra *)
assert (avg_vote alf2023 = 25.6);;

print_endline "Tutti i test (ignorando l'ordine) sono stati superati!";;
