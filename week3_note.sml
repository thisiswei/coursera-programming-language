fun double x = 2 * x
fun incr x = x + 1
val a_tuple = (double, incr, double(incr 7))
val eighteen = (#1 a_tuple) 9

fun n_times(f, n, x) =
    if n=0
    then x
    else f (n_times(f, n-1, x))

fun incrment x = x + 1
fun double x = 2 * x

fun addition(n, x) = n_times(incrment, n, x)
fun double_n(n, x) = n_times(double, n, x)
fun nth_tail(n, x) = n_times(tl, n, x)
fun triple_n_times(n, x) = n_times(fn x => x*3, n, x)

