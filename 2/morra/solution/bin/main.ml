type winner = Player | Computer | Tie;;

Random.self_init;;

let win (hp,gp) = 
  let hc = Random.int 6 in
  let gc = Random.int 11 in
  if gp = (hp+hc) && gp != gc then ((hc,gc), Player) else
  if gc = (hp+hc) && gc != gp then ((hc,gc), Computer) else
    ((hc,gc), Tie);;
