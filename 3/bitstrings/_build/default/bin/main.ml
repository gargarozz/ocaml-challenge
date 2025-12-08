type bitstring = E | Z of bitstring | U of bitstring;;

let string_of_bitstring b = 
  let rec loop acc b = match b with 
    | E -> acc
    | Z(n) -> loop (acc^"0") n
    | U(n) -> loop (acc^"1") n
  in loop "" b
;;

let len b = 
  let rec loop acc b = match b with
    | E -> acc
    | Z(n) | U(n) -> loop (acc+1) n
  in loop 0 b
;;

let countZ b =
  let rec loop acc b = match b with
    | E -> acc
    | Z(n) -> loop (acc+1) n
    | U(n) -> loop acc n
  in loop 0 b
;;

let countU b =
  let rec loop acc b = match b with
    | E -> acc
    | U(n) -> loop (acc+1) n
    | Z(n) -> loop acc n
  in loop 0 b
;;

let rec concat a b = match a with
  | E -> b
  | Z(s) -> Z (concat s b)
  | U(s) -> U (concat s b)
;;

let equals a b = (a = b);;

let tl = function
  | E -> E
  | U(t) -> t
  | Z(t) -> t
;;

let rec prefix s1 s2 = match s1,s2 with
  | E,_ -> true
  | U(n),U(m) | Z(n),Z(m) -> prefix n m
  | _ -> false
;;

let rec substring s1 s2 = match s1,s2 with
  | _,E -> false
  | _,U(s2') | _,Z(s2') -> if prefix s1 s2 then true else substring s1 s2'
;;

let () = 
  (* s1 = "01" *)
  let s1 = Z (U E) in 
  (* s2 = "10" *)
  let s2 = U (Z E) in 
  (* s3 = "010" *)
  let s3 = Z (U (Z E)) in 

  (* === 1. Test string_of_bitstring === *)
  assert (string_of_bitstring E = "");
  assert (string_of_bitstring (Z E) = "0");
  assert (string_of_bitstring (U E) = "1");
  assert (string_of_bitstring s1 = "01");
  assert (string_of_bitstring s3 = "010");

  (* === 2. Test len === *)
  assert (len E = 0);
  assert (len (Z E) = 1);
  assert (len s1 = 2); (* "01" lung 2 *)
  assert (len s3 = 3); (* "010" lung 3 *)

  (* === 3. Test countZ e countU === *)
  (* countZ *)
  assert (countZ E = 0);
  assert (countZ (U (U E)) = 0); (* "11" -> zero 0s *)
  assert (countZ s1 = 1);        (* "01" -> uno 0 *)
  assert (countZ s3 = 2);        (* "010" -> due 0s *)

  (* countU *)
  assert (countU E = 0);
  assert (countU (Z (Z E)) = 0); (* "00" -> zero 1s *)
  assert (countU s1 = 1);        (* "01" -> uno 1 *)
  assert (countU (U (U (U E))) = 3);

  (* === 4. Test concat === *)
  (* E + s1 = s1 *)
  assert (equals (concat E s1) s1);
  (* s1 + E = s1 *)
  assert (equals (concat s1 E) s1);
  (* "0" + "1" = "01" *)
  assert (equals (concat (Z E) (U E)) s1); 
  (* "01" + "0" = "010" *)
  assert (equals (concat s1 (Z E)) s3); 

  (* === 5. Test equals === *)
  assert (equals E E = true);
  assert (equals s1 s1 = true);
  assert (equals (Z E) (Z E) = true);
  assert (equals (Z E) (U E) = false); (* 0 != 1 *)
  assert (equals s1 s2 = false);       (* 01 != 10 *)
  assert (equals s1 s3 = false);       (* Lunghezze diverse *)
  assert (equals E (Z E) = false);

  (* === 6. Test tl (tail) === *)
  assert (equals (tl E) E);            (* Tail di vuoto è vuoto *)
  assert (equals (tl (Z E)) E);        (* Tail di "0" è vuoto *)
  assert (equals (tl s1) (U E));       (* Tail di "01" è "1" *)
  assert (equals (tl s3) (U (Z E)));   (* Tail di "010" è "10" *)

  (* === 7. Test prefix === *)
  (* E è prefisso di tutto *)
  assert (prefix E s1 = true);
  assert (prefix E E = true);
  (* Una stringa è prefisso di se stessa *)
  assert (prefix s1 s1 = true);
  (* "0" è prefisso di "01" *)
  assert (prefix (Z E) s1 = true);
  (* "01" è prefisso di "010" *)
  assert (prefix s1 s3 = true);

  (* Casi False *)
  (* "1" non è prefisso di "01" *)
  assert (prefix (U E) s1 = false);
  (* "010" (lunga) non è prefisso di "01" (corta) *)
  assert (prefix s3 s1 = false);

  (* === 8. Test substring === *)
  (* E è sottostringa di tutto *)
  assert (substring E s1 = true);
  (* Una stringa è sottostringa di se stessa *)
  assert (substring s1 s1 = true);

  (* "1" è dentro "01" (è in coda) *)
  assert (substring (U E) s1 = true);
  (* "1" è dentro "010" (è nel mezzo) *)
  assert (substring (U E) s3 = true);
  (* "01" è dentro "010" (è all'inizio) *)
  assert (substring s1 s3 = true);
  (* "10" è dentro "010" (è alla fine) *)
  assert (substring (U (Z E)) s3 = true);

  (* Casi False *)
  (* "11" non è dentro "010" *)
  assert (substring (U (U E)) s3 = false);
  (* "000" non è dentro "01" *)
  assert (substring (Z (Z (Z E))) s1 = false);

  print_endline "Tutti i test passati con successo!";;
