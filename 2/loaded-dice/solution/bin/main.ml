Random.self_init ();;

let dice prob6 = 
  let randNum = (Random.int 100)+1 in
  if randNum < prob6 then 6 else (
    let step = (100 - prob6)/5 in
    if randNum < prob6+step then 5 else
    if randNum < prob6+(step*2) then 4 else
    if randNum < prob6+(step*3) then 3 else
    if randNum < prob6+(step*4) then 2 else
    1
  );;

