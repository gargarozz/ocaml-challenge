type nat = Z | S of nat;;

let rec iseven = function
  | Z -> true
  | S(S(a)) -> iseven a
  | _ -> false
;;

let rec halve n = match n with
  | Z -> Z
  | S(Z) -> Z
  | S(S(a)) -> S(halve a)
;;

let rec add a b = match a with
  | Z -> b
  | S(n) -> add n (S(b))
;;

let mul a b =
  let rec loop acc a b = match b with
    | Z -> acc
    | S(b') -> loop (add a acc) a b'
  in loop Z a b
;;

let rec leq a b = match a,b with
  (* passi base: uguali o a strettamente minore minore di b con differenza di uno *)
  | n,m when n=m -> true
  | n,(S(m)) when n=m -> true
  (* passo base: a non e' zero e b e' zero *)
  | n, Z -> false
  (* passo ricorsivo: sottrai uno a b e riprova *)
  | n,(S(m)) -> leq a m
;;

let equals a b = (a = b);;
