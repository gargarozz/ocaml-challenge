let enum_int x = if x mod 2 = 1 then ((x/2)+1)*(-1) else x/2;;

let () = assert (List.init 10 enum_int = [0; -1; 1; -2; 2; -3; 3; -4; 4; -5]);;
