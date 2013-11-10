(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun all_exception_option (s, []) = NONE
  | all_exception_option (s, x::xs) =
  case same_string(s, x) of
       true => SOME xs
     | false => case all_exception_option(s, xs) of
                     NONE => NONE
                   | SOME y => SOME (x::y)

fun get_substitutions1([], s) = []
  | get_substitutions1(x::xs, s) =
  case all_exception_option (s, x) of
       NONE => get_substitutions1(xs, s)
     | SOME y => y @ get_substitutions1(xs, s)

fun get_substitutions2([], s) = []
  | get_substitutions2(x::xs, s) =
  let fun aux(lst, acc) =
          case lst of
               [] => acc
             | x::xs => case all_exception_option(s, x) of
                             NONE => aux(xs, acc)
                           | SOME x' => aux(xs, (x' @ acc))
  in aux(x::xs, []) end

fun similar_names(lst, name) =
    let val {first=f, middle=m, last=l} = name
        fun helper(xs) =
            case xs of
                 [] => [name]
               | x::xs' => {first=x, middle=m, last=l} :: helper(xs')
    in
        helper(get_substitutions2(lst, f))
    end


