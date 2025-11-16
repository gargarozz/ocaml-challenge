let sumrange a b =
  let rec tail_sumrange acc a b =
    if a > b then acc
    else tail_sumrange (acc+a) (a+1) b in
  tail_sumrange 0 a b;;
