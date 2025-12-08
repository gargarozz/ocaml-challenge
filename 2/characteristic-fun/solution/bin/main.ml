let f2 x = if x < 0 then failwith "undefined" else (if x<=2 then 1 else 0)

let f3 x = if x < 0 then failwith "undefined" else (if x<=100 then 1 else 0)

let f4 x = if x < 0 then failwith "undefined" else 0

let f5 x = if x < 0 then failwith "undefined" else 1

let f6 x = if x < 0 then failwith "undefined" else (if (x mod 2 = 0) then 1 else 0)

let f7 x = if x < 0 then failwith "undefined" else (if (x mod 2 = 0) then 1 else 0)

let f8 x = if x < 0 then failwith "undefined" else 1

let f9 x = if x < 0 then failwith "undefined" else 1

let f10 x = if x < 0 then failwith "undefined" else (if x >= 4 && x <= 19 then 1 else 0)

let f11 x = if x < 0 then failwith "undefined" else (if x <= 50 && x mod 2 = 0 then 1 else 0)

(* ... *)

let f15 x = if x < 0 then failwith "undefined" else (
  let rec has_div x n =
    if n>=x then
      false
    else 
      if (x mod n) = 0 then
        true
      else
        false
  in not (has_div x 2)
  )
;;
