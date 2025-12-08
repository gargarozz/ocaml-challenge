let rec gcd a b = if a = b then a else
  if a > b then gcd (a-b) b else gcd (b-a) a
;;

let () = 
  assert (gcd 5 5 = 5);
  assert (gcd 1 1 = 1);
  assert (gcd 100 100 = 100);

  (* === 2. Uno è multiplo dell'altro === *)
  assert (gcd 10 5 = 5);
  assert (gcd 5 10 = 5); (* Proprietà commutativa *)
  assert (gcd 20 4 = 4);
  assert (gcd 4 20 = 4);

  (* === 3. Numeri Coprimi (MCD = 1) === *)
  assert (gcd 3 7 = 1);
  assert (gcd 13 17 = 1);
  assert (gcd 9 16 = 1); (* Non sono primi, ma sono coprimi tra loro *)

  (* === 4. Casi Generici === *)
  (* Esempio che abbiamo fatto prima: 20 e 12 -> 8,12 -> 8,4 -> 4,4 -> 4 *)
  assert (gcd 12 20 = 4); 
  assert (gcd 24 18 = 6);
  assert (gcd 30 45 = 15);
  assert (gcd 60 48 = 12);

  (* === 5. Casi con 1 (L'elemento neutro moltiplicativo) === *)
  assert (gcd 1 50 = 1);
  assert (gcd 100 1 = 1);

  (* === 6. Numeri un po' più grandi (Opzionale) === *)
  assert (gcd 105 252 = 21);

  print_endline "Tutti i test del MCD sono passati!";;
