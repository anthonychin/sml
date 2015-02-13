(* Anthony Wei-Wing Chin *)
(*       260482009       *)

Control.Print.printDepth := 100; 

(* Problem 1 *)

(* Helper *)
fun remove (e: ''a, lst: ''a list) : ''a list =
  case lst of
    [] => []
    | y :: ys => if e=y 
                      then remove(e, ys)
                      else y :: remove(e, ys)

fun remDuplicates (lst: ''a list) : ''a list =
  case lst of
    [] => []
    | x :: xs => x :: remDuplicates(remove(x, xs))

(* Problem 2 *)
exception Diverge

(* Helper *)
fun derivative f x delta =
  (f (x + delta) - f (x - delta)) / (2.0 * delta)

fun loop f x eps ctr : real =
  if abs(f(x)) < eps then x
  else if ctr > 1000 then raise Diverge
  else loop f (x - (f(x)/derivative f x eps)) eps (ctr +1)

(* Loop 1000 times and keep computing f(x) and compare to eps *)
fun newton (eps: real, f: real -> real, x: real) : real =
  if abs(f(x)) < eps then x
  else
    loop f x eps 0

(* Problem 3 *)
(* Helper *)
fun sum (f: real -> real, a: real, b: real, inc: real->real) =
  if (a > b) then 0.0
  else (f a) + sum (f, inc(a), b, inc)

fun integral (f: real -> real, a: real, b: real, dx: real) =
  dx * sum (f, a + (dx/2.0), b, fn x => x+dx)


fun integrate (f: real -> real, g: real -> real, dy: real) x : real =
  integral(fn y => f(y) * g(x + y), 0.0, x, dy)

(* Problem 4 *)
(* Helper *)
datatype Mathexp =
  Num of int
  | Var of string
  | Neg of Mathexp
  | Add of Mathexp * Mathexp
  | Mul of Mathexp * Mathexp

fun diff (m: Mathexp, x: string) : Mathexp =
  case m of
    Num _ => Num 0
    | Var t => if t=x then Num 1 else Num 0
    | Neg m => Neg (diff(m, x))
    | Add (m1, m2) => Add (diff(m1, x), diff(m2, x))
    | Mul (m1, m2) => Add (Mul (diff(m1, x), m2), Mul (m1, (diff(m2, x)))) (* Product Rule *)

(* Keep recursing and then handle simple base cases. ex Num 0 => 0 for Mul, Num 0 => math exp for Add etc.*)
fun simplify (m: Mathexp) : Mathexp =
  case m of
    Add (m1, m2) => ( case (simplify(m1), simplify(m2)) of
                      (Num 0, _) => simplify(m2)
                      |(_, Num 0) => simplify(m1)
                      |(Num a, Num b) => Num (a+b)
                      |(_, _) => Add ( simplify(m1), simplify(m2) ) )
    | Mul (m1, m2) => ( case (simplify(m1), simplify(m2)) of
                      (Num 0, _) => Num 0
                      |(_, Num 0) => Num 0
                      |(Num 1, _) => simplify(m2)
                      |(_, Num 1) => simplify(m1)
                      |(Num a, Num b) => Num (a*b)
                      |(m1, m2) => Mul ( simplify(m1), simplify(m2) ))
    | Neg (Num n) => if n > 0 then Num (~n) else Num(n)
    | _ => m
