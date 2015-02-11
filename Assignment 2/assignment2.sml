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
(*val sol2 = newton2(eqn2, 2.0, 0.001)*)

(* Problem 3 *)