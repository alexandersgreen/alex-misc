type x = X
       | PowerX of int 

type expression = Var of x
                | Val of int 
                | Add of expression * expression
                | Minus of expression * expression
                | Multiply of expression * expression
                | Divide of expression * expression

(* val power' : int -> x*)
let power' n =
   match n with
     1 -> X
   | x -> PowerX x 

(* val dif' : x -> expression *)
let dif' x =
   match x with
     X -> Val 1
   | PowerX n -> Multiply (Val n, Var (power' (n-1)))


(* val dif : expression -> expression *)
let rec difX expr =
   match expr with
     Var x -> dif' x
   | Val n -> Val 0 
   | Add (f,g) -> Add (difX f,difX g)
   | Minus (f,g) -> Minus (difX f, difX g)
   | Multiply (f,g) -> Add (Multiply (difX f,g),Multiply (f,difX g))
   | Divide (f,g) -> Divide (Minus (Multiply (g,difX f),Multiply (f,difX g)),Multiply (g,g))

let rec pow' n e =
   match e with
     1 -> n
   | x -> n * (pow' n (x-1))

let evalVar x n = 
   match x with
      X -> n
    | PowerX e ->  pow' n e

(* val evalX : expression -> int -> int *)
let rec evalX expr n =
   match expr with
     Var x -> evalVar x n
   | Val v -> v
   | Add (f,g) -> (evalX f n) + (evalX g n)
   | Minus (f,g) -> (evalX f n) - (evalX g n)
   | Multiply (f,g) -> (evalX f n) * (evalX g n)
   | Divide (f,g) -> (evalX f n) / (evalX g n)

