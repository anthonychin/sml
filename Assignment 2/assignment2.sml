(* Problem 1 *)
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

fun derivative f x delta =
  (f (x + delta) - f (x - delta)) / (2.0 * delta)

fun loop f x eps ctr: real =
  if abs(f(x)) < eps then x
  else if ctr > 1000 then raise Diverge
  else loop f (x - (f(x)/derivative f x eps)) eps (ctr +1)


fun newton (eps: real, f: real -> real, x: real) : real =
  if abs(f(x)) < eps then x
  else
    loop f x eps 0

val eqn1 = fn x => x*x*x + 2.0*x - 2.0;
val eqn2 = fn x => x*x - 2.0;

val sol = newton(0.001, eqn2, 2.0)

(* Problem 3 *)
fun sum (f: real -> real, a: real, b: real, inc: real->real) =
  if (a > b) then 0.0
  else (f a) + sum (f, inc(a), b, inc)

fun integral (f: real -> real, a: real, b: real, dx: real) =
  dx * sum (f, a + (dx/2.0), b, fn x => x+dx)

(*fun integrate (f: real -> real, g: real -> real, dy: real) x : real =
  dy * sum (fn y => f(y) * g(x + y), dy/2.0, x, fn inc => inc + dy)*)

fun integrate2 (f: real -> real, g: real -> real, dy: real) x : real =
  integral(fn y => f(y) * g(x + y), 0.0, x, dy)

val sol2 = integrate(eqn1, eqn2, 0.001)

