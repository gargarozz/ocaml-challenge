let first_third_fifth = function
  | a::b::c::d::e::_ -> Some (a,c,e)
  | _ -> None
;;
