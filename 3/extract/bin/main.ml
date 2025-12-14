  (*
      caso errore: lista vuota index out of bounds
      caso base: i=0 l'elemento e' stato trovato! rimuovilo dalla lista e restituisci elemento e nuova lista 
      passo ricorsivo: i non e' zero -> decrementa i e continua a cercare 
  *)
let extract i l =
  let rec helper acc i l =
    match l with
      | [] -> raise (Failure "index out of bounds")
      | x::xs -> if i = 0 then (x, (acc@xs)) else helper (acc@[x]) (i-1) xs
  in helper [] i l
;;
