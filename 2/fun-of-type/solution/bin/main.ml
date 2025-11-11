let f1 x = x > 5

let f2 b = if b then 1 else 2

let f3 x = (x,x>5)

let f4 ((x:int),(b:bool)) = x

let f5 x = (x, x*2)

let f6 x b = if b then x else x*2

let f7 b x = if x>5 then b else not b

let f8 b1 b2 = if b1 && b2 then 1 else 2

let f9 b x = if b then x+1 else x+2

let f10 (f:int -> int) = f 4

let f11 (f:int -> bool) = if f 4 then 4 else 3

let f12 (f:bool -> int) = if (f true) > 5 then 5 else 4

let f13 (f:int -> bool) = f 3

let f14 (f:bool -> bool) = if f true then 4 else 3

let f15 x (y,z) = x + y + z

let f16 x y z = x + y + z

let f17 (f:int -> int) x = f x

let f18 (f:(int -> int) -> int) = f (fun x -> x+1) 

let f19 (f:int -> int) b = if b then f 3>3 else f 4<3

let f20 (f:int -> bool) b = if b && f 4 then 4 else 5

