Random.self_init();;

let rec rnd_list n b = if n <= 0 then [] else (* passo base, n e' 0 *)
  let num = (Random.int b) + 1 in (* genera numero casuale *)
  num::rnd_list (n-1) b (* chiama ricorsivamente rnd_list e concatena il numero ottenuto *)
