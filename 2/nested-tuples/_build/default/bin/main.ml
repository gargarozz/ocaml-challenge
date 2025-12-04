let total_score (name, (written, oral)) = written + oral;;

let passed (name, (written, oral)) = (written >= 18) && (oral >= 18);;

let string_of_result (name, (written, oral)) = 
  if passed (name, (written, oral)) then name^" passed with total "^(string_of_int (written+oral)) else name^" did not pass";;

let merge (n1, (w1,o1)) (n2, (w2,o2)) = 
  let maxw = if w1 > w2 then w1 else w2 in
  let maxo = if o1 > o2 then o1 else o2 in
  if n1 = n2 then (n1, (maxw, maxo)) else failwith "different students"

let () = 
  (* === TEST total_score === *)
  (* Verifica la somma semplice *)
  assert (total_score ("Mario", (20, 10)) = 30);
  assert (total_score ("Luigi", (0, 0)) = 0);
  assert (total_score ("Anna", (30, 30)) = 60);

  (* === TEST passed === *)
  (* Caso 1: Passato abbondantemente *)
  assert (passed ("Mario", (20, 20)) = true);

  (* Caso 2: Bocciato (scritto insufficiente) *)
  assert (passed ("Mario", (17, 30)) = false);

  (* Caso 3: Bocciato (orale insufficiente) *)
  assert (passed ("Mario", (30, 17)) = false);

  (* Caso 4: Bocciato (entrambi insufficienti) *)
  assert (passed ("Mario", (10, 10)) = false);

  (* Caso 5: CASO LIMITE (Esattamente 18) - Fondamentale! *)
  (* La tua logica usa >= 18, quindi 18 deve passare *)
  assert (passed ("Mario", (18, 18)) = true);


  (* === TEST string_of_result === *)
  (* Caso Passato *)
  assert (string_of_result ("Luca", (20, 20)) = "Luca passed with total 40");

  (* Caso Bocciato *)
  (* Nota: nel ramo else stampi solo il nome + " did not pass" *)
  assert (string_of_result ("Giulia", (10, 10)) = "Giulia did not pass");


  (* === TEST merge === *)
  (* Caso 1: Primo tentativo migliore nello scritto, secondo nell'orale *)
  (* ("Test", (20, 10)) unito a ("Test", (15, 25)) 
     deve dare max scritto (20) e max orale (25) -> (20, 25) *)
  assert (merge ("Test", (20, 10)) ("Test", (15, 25)) = ("Test", (20, 25)));

  (* Caso 2: Secondo tentativo migliore in tutto *)
  assert (merge ("Test", (18, 18)) ("Test", (30, 30)) = ("Test", (30, 30)));

  (* Caso 3: Studenti diversi (Deve fallire) *)
  assert (
    try 
      merge ("Mario", (20, 20)) ("Luigi", (20, 20)) |> ignore; 
      false 
    with Failure msg -> msg = "different students"
  );

  print_endline "Tutti i test sono passati correttamente!";;
