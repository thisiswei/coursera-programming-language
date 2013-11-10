(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

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

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


fun card_color(suit, rank) =
    case suit of
         Hearts => Red
       | Spades => Red
       | _ => Black

fun card_value(suit, rank) =
    case rank of
         Num i => i
       | Ace => 11
       | _ => 10

(* Write a function remove_card, which takes a list of cards cs, a card c, and
an exception e. It returns a list that has all the elements of cs except c. If
c is in the list more than once, remove only the first one. If c is not in the
list, raise the exception e. You can compare cards with =.*)

fun remove_card (cs, c, e) =
    case cs of
         [] => []
       | cs'::cs'' => case all_exception_option(cs', c) of
                           NONE => raise e
                         | SOME cards => cards @ remove_card(cs'', c, e)

(* (d) Write a function all_same_color, which takes a list of cards and returns
true if all the cards in the list are the same color. Hint: An elegant
solution is very similar to one of the functions using nested pattern-matching
in the lectures.*)

fun all_same_color([]) = true
  | all_same_color(x::[]) = true
  | all_same_color(x::xs::xs') =
  card_color(x) = card_color(xs) andalso all_same_color(xs')

(* (e) Write a function sum_cards, which takes a list of cards and returns the
sum of their values. Use a locally defined helper function that is tail
recursive.*)

fun sum_cards(cs) =
    let fun aux(cs, acc) =
            case cs of 
                 [] => acc
               | c::cs' => aux(cs', card_value(c) + acc)
    in
        aux(cs, 0)
    end

(* (f) Write a function score, which takes a card list (the held-cards) and an
int (the goal) and computes the score as described above. *)

(*
(g) Write a function officiate, which “runs a game.” It takes a card list (the
card-list) a move list (what the player “does” at each point), and an int (the
goal) and returns the score at the end of the game after processing (some or all
of) the moves in the move list in order. Use a locally
defined recursive helper function that takes several
arguments that together represent the current state of
the game. As described above: *)
(*
• The game starts with the held-cards being the empty list.
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove exception.
• If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues with a larger held-cards and a smaller card-list.
Sample solution for (g) is under 20 lines.
*)
