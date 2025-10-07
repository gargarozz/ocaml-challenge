type vote = StrongReject | WeakReject | WeakAccept | StrongAccept;;

let count f lst = 
        List.fold_left (fun acc x -> if f x then acc + 1 else acc) 0 lst;;

let decide_exam v1 v2 v3 = 
        let votes = [v1; v2; v3] in
        let accepts = 
                count (function WeakAccept | StrongAccept -> true | _ -> false) votes
        in
        let has_strong_reject =
                List.exists ((=) StrongReject) votes
        in
        accepts >= 2 && not has_strong_reject;;

let () = print_endline "Hello, World!"
