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


