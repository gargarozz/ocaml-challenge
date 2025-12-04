let bounce n x = 
  let r = x mod (2*n) in
  if r < n then r else (2*n)-r
;;
