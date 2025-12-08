let rec lang0 w = 
  match w with
    | [] -> true
    | [0] -> false
    | [_] -> true
    | h::t -> lang0 t
;;

let rec lang1 w =
  let rec no_zeros w =
    match w with 
      | [] -> true
      | 1::xs -> no_zeros xs
      | 0::_ | _ ->  false
  in match w with
    | [] -> true
    | 1::xs -> lang1 xs
    | 0::xs -> no_zeros xs
    | _ -> false
;;

(* lang2 e' il linguaggio delle parole composte da {0,1,2} 
   per cui dopo un 1 non possono esserci zeri *)
let rec lang2 w =
  let rec no_zeros w = 
    match w with 
      | [] -> true
      | 0::xs -> false
      | _::xs -> no_zeros w 
  in match w with
    | [] -> true
    | 1::xs -> no_zeros
    | _::xs -> lang2 xs
;;

(* lang3 e' il linguaggio delle parole in cui ogni 0 e' seguito da 11 *)
let rec lang3 w =
  match w with
    | [] -> true
    | 0::1::1::xs -> lang3 xs
    | 0::_ -> false
    | _::xs -> lang3 xs
;;

(* lang4 e' il linguaggio delle parole in {0,1} in cui la quantita' di zeri e'
   maggiore o uguale rispetto alla quantita' di uni *)
let lang4 w =
  let rec loop n0 n1 w =
    match w with
      | [] -> n0>=n1
      | 0::xs -> loop (n0+1) n1 xs
      | 1::xs -> loop n0 (n1+1) xs
  in loop 0 0 w
;;

(* lang5 e' il linguaggio delle parole in {0,1} in cui il numero di zeri e'
   uguale al numero di uni *)
let lang5 w =
  let rec loop diff w = 
    match w with
      | [] -> diff = 0
      | 0::xs -> loop (diff-1) xs
      | 1::xs -> loop (diff+1) xs
  in loop 0 w
;;

(* lang6 e' il linguaggio delle parole composte da un numero n di 0 seguiti
   da un numero uguale n di 1 *)
let lang6 w = 
  let rec loop1 n w =
    match w with
      | [] -> n = 0
      | 0::xs -> false
      | 1::xs -> if n > 0 then loop1 (n-1) xs else false
  in
  let rec loop0 n w =
    match w with
      | [] -> n = 0
      | 1::xs -> loop1 (n-1) xs
      | 0::xs -> loop0 (n+1) xs
  in loop0 0 w
;;

(* lang7 e' il linguaggio delle parole composte da un numero n di 0 seguiti
   da un 1 e poi da numero uguale n di 0 *)
let lang7 w = 
  let rec loop1 n w =
    match w with
      | [] -> n = 0
      | 1::xs -> false
      | 0::xs -> if n > 0 then loop1 (n-1) xs else false
  in
  let rec loop0 n w =
    match w with
      | [] -> false
      | 1::xs -> loop1 n xs
      | 0::xs -> loop0 (n+1) xs
  in loop0 0 w
;;

(* lang8 e' il linguaggio delle parole in {0,1,2} in cui dopo il primo uno
   il numero di 2 e' maggiore del numero di 0 *)
let lang8 w = 
  let rec afterone diff w =
    match w with
      | [] -> diff = 0
      | 0::xs -> afterone (diff+1) xs
      | 2::xs -> afterone (diff-1) xs
      | _::xs -> afterone diff xs
  in
  let rec beforeone w =
    match w with
      | [] -> true
      | 1::xs -> afterone 0 xs
      | _::xs -> beforeone xs
  in beforeone w
;;

(* lang9 definisce un linguaggio uguale a lang6 *)
let lang9 w = 
  let rec loop1 n w =
    match w with
      | [] -> n = 0
      | 0::xs -> false
      | 1::xs -> if n > 0 then loop1 (n-1) xs else false
  in
  let rec loop0 n w =
    match w with
      | [] -> n = 0
      | 1::xs -> loop1 (n-1) xs
      | 0::xs -> loop0 (n+1) xs
  in loop0 0 w
;;

