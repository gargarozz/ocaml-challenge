let enum_nat_nat n =
  (* Funzione ricorsiva interna *)
  (* n_restante: quanto rimane del numero originale *)
  (* somma_curr: la somma x+y della diagonale che stiamo controllando *)
  let rec cerca_diagonale n_restante somma_curr =
    let elementi_nella_diag = somma_curr + 1 in
    
    if n_restante < elementi_nella_diag then
      (* CASO BASE: Siamo nella diagonale giusta! *)
      (* La x è semplicemente il resto che ci è avanzato *)
      let x = n_restante in
      let y = somma_curr - x in
      (x, y)
    else
      (* PASSO RICORSIVO: Il numero è ancora troppo grande. *)
      (* Sottraiamo la lunghezza di questo gruppo e passiamo al prossimo (somma + 1) *)
      cerca_diagonale (n_restante - elementi_nella_diag) (somma_curr + 1)
  in
  
  (* Avviamo la ricerca partendo dal numero originale n e dalla somma 0 *)
  cerca_diagonale n 0
;;
