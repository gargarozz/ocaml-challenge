type grade = Val of in | CumLaude;;

let is_valid g = ((g >= 18) && (g <= 30)) || g = CumLaude;;

let int_of_grade = function
  | Val(n) -> if (n >= 18 && n <= 30) then n else failwith "not valid"
  | CumLaude -> 32

let avg = function 
  | [] -> raise Empty_list (* lista vuota *)
  | hd::l -> (* lista non vuota *)
      let rec aux average count = function
        | hd::l -> aux ((average*.(float (count-1))+.(int_of_grade hd))/.(float (count))) (count+1) l
        | _ -> average
      in aux hd 1 l
;;

let avg_norec grades = 
  grades |> List.filter is_valid |> List.map int_of_grade |> fun scores -> (* filtra i validi e trasforma in int*)
    let sum = List.fold_left (+) 0 scores in (* calcola somma dei voti *)
    let count = List.length scores in (* calcola lunghezza lista*)
    if count 0 then 0.0 else (float_of_int sum) /. (float_of_int count) (* se e' 0 restituisci 0.0 altrimenti calcola media *)
;;
