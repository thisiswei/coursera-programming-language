datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

val a = Str "hi"
val b = Str
val c = Pizza
val d = TwoInts(1+2, 3+4)
val e = a

fun f x =
    case x of
         Pizza => 3
       | Str s => 8
       | TwoInts(i1, i2) => i1 + i2

datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int
datatype id = StudentNum of int
            | Name of string

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun eval (Constant i) = i
  | eval (Negate e2) = ~ (eval e2)
  | eval (Add(e1, e2)) = (eval e1) + (eval e2)
  | eval (Multiply(e1, e2)) = (eval e1) * (eval e2)

fun append ([], ys) = ys
  | append (x::xs', ys) = x :: append(xs', ys)

fun eval e =
    case e of
         Constant i => i

       | Negate e2 => ~ (eval e2)
       | Add(e1, e2) => (eval e1) + (eval e2)
       | Multiply(e1, e2) => (eval e1) * (eval e2)

fun number_of_adds e =
    case e of
         Constant i => 0
       | Negate e2 => number_of_adds e2
       | Add (e1, e2) => 1 + number_of_adds e1 + number_of_adds e2
       | Multiply (e1, e2) => number_of_adds e1 * number_of_adds e2

val example_exp = Add ((Constant 19), Negate(Constant 4))

fun max_constant e =
    case e of
         Constant i => i
       | Negate e2 => max_constant e2
       | Add(e1, e2) => Int.max(max_constant e1, max_constant e2)
       | Multiply(e1, e2) => Int.max(max_constant e1, max_constant e2)

(* type name = t *)

type card = suit * rank

type name_record = { student_num : int option,
                     first       : string,
                     middle      : string option,
                     last        : string }

fun is_queen_of_spaces (c : card) =
    #1 c = Spade andalso #2 c = Queen

fun is_queen_of_spades2 c =
    case c of
         (Spade, Queen) => true
       | _ => false


datatype my_int_list = Empty
                    | Cons of int * my_int_list

val x = Cons(4, Cons(23, Empty))

fun append_my_list (xs, ys) =
    case xs of
         Empty => ys
       | Cons(x, xs') => Cons(x, append_my_list(xs', ys))

fun sum_list xs =
    case xs of
         [] => 0
       | x::xs' => x + sum_list xs'

fun append_list (xs, ys) =
    case xs of
         [] => ys
       | x::xs' => x :: append_list(xs', ys)

fun sum_triple triple =
    case triple of
         (x, y, z) => x + y + z

fun sum_triple_good triple =
    let val (x, y, z) = triple (* pattern matching with val binding*)
    in
        x + y + z
    end

fun sum_triple_good (x, y, z) = (* function param can be pattern as well *)
    x + y + z

fun full_name r =
    case r of
         {first=x, middle=y, last=z} =>
         x ^ " " ^ y ^ " " ^ z

fun full_name_good r =
    let val {first=x, middle=y, last=z} = r
    in x ^ " " ^ y ^ " " ^ z end

fun full_name_great {first=x, middle=y, last=z} =
    x ^ " " ^ y ^ " " ^ z

(* val p = e patten to expression *)

exception wtf

fun zip2 list_triple =
    case list_triple of
         ([], [], []) => []
       | (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3) :: zip2(tl1, tl2, tl3)
       | _ => raise wtf

fun unzip2 lst =
    case lst of
         [] => ([], [], [])
       | (a, b, c) :: tl => let val (v1, v2, v3) = unzip2 tl
                            in (a :: v1, b :: v2, c::v3) end

(* More nested pattern matching *)

fun nondescreasing xs =
    case xs of
         [] => true
       | _::[] => true
       | x::(neck::rest) => x<=neck andalso nondescreasing (neck::rest)

datatype sgn = N | P | Z
fun multsign(x1, x2) =
    let fun sign x = if x=0 then Z else if x < 0 then N else P
    in
        case (sign x1, sign x2) of
             (Z, _) => Z
           | (_, Z) => Z
           | (P, P) => P
           | (N, N) => P
           | _ => N
    end

fun hd x =
    case x of
         [] => raise List.Empty
       | x::_ => x

exception Myexception
exception Myexception2 of int * int

fun mydiv(x, y) =
    if y=0
    then raise Myexception
    else x div y

fun maxlist(xs, ex) =
    case xs of 
         [] => raise ex
       | x::[] => x
       | x::xs' => Int.max(x, maxlist(xs', ex))
(* e1 handle ex => e2 *)
(* if e1 raise exception, we catch that, then evaluate e2 *)

val x = maxlist([], Myexception)
    handle Myexception => 52

fun fact n =
    let fun aux(n, acc) =
            if n=0
            then acc
            else aux(n-1, acc*n)
    in
        aux(n, 1)
    end

fun sum2 xs =
    let fun aux(xs, acc)=
            case xs of
                 [] => acc
               | x::xs' => aux(xs', acc+x)
    in
        aux(xs, 0)
    end

fun rev xs =
    case xs of
         [] => []
       | x::xs' => (rev xs') @ [x]

fun rev2 xs =
    let fun aux(xs, acc) =
            case xs of
                 [] => []
               | x::xs' => aux(xs', x::acc)
    in
        (xs, [])
    end


