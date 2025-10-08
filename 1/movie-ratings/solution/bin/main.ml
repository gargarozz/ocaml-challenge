type rating = Masterpiece | HighlyRecommended | Recommended | MixedReviews;;

let count f lst = 
        List.fold_left (fun acc x -> if f x then acc + 1 else acc) 0 lst

let movie_rating r1 r2 r3 = 
        let ratings = [r1; r2; r3] in
        let fives = count (fun a -> a = 5) ratings in
        let fours = count (fun a -> a >= 4) ratings in
        let threes = count (fun a -> a >= 3) ratings in
        if (count (fun a -> a > 5 || a < 1) ratings) > 0 then failwith "error" else
        match fives, fours, threes with
                | 3, 3, 3 -> Masterpiece
                | 2, 3, 3 -> HighlyRecommended
                | _, 2, 3 -> Recommended
                | _, _, _ -> MixedReviews;;


let () = print_endline "Hello, World!"
