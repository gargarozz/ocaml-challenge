type operation = Add of int * int | Sub of int * int | Mul of int * int | Div of int * int

let eval = function
  | Add(a,b) -> Some(a+b)
  | Sub(a,b) -> Some(a-b)
  | Mul(a,b) -> Some(a*b)
  | Div(a,b) -> if b = 0 then None else Some(a/b);;

let make_operation op a b = match op with
  | '+' -> Add(a,b)
  | '-' -> Sub(a,b)
  | '*' -> Mul(a,b)
  | '/' -> Div(a,b)
  | _ -> failwith "undefined operation";;

let () = 
  assert (make_operation '+' 2 3 = Add (2, 3));
  assert (make_operation '-' 10 1 = Sub (10, 1));
  assert (make_operation '*' 4 4 = Mul (4, 4));
  assert (make_operation '/' 20 2 = Div (20, 2));

  assert (eval (Add (3, 4)) = Some 7);
  assert (eval (Add (-1, 1)) = Some 0);
  assert (eval (Sub (10, 4)) = Some 6);
  assert (eval (Sub (3, 5)) = Some (-2));
  assert (eval (Mul (3, 3)) = Some 9);
  assert (eval (Mul (3, 0)) = Some 0);
  assert (eval (Div (10, 2)) = Some 5);
  assert (eval (Div (10, 3)) = Some 3); 
  assert (eval (Div (5, 0)) = None);;
