(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_exception_option (s, []) = NONE
  | all_exception_option (s, x::xs) =
  case s=x of
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
    case all_exception_option(c, cs) of
         NONE => raise e
       | SOME cards => cards

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
int (the goal) and computes the score as described below

Scoring works as follows: Let sum be the sum of the values of the held-cards.
* If sum is greater than goal, the preliminary score is three times (sum−goal),
* else the preliminary score is (goal − sum). The score is the preliminary score
* unless all the held-cards are the same color, in which case the score is the
* preliminary score divided by 2 (and rounded down as usual with integer
* division; use ML’s div operator) *)

fun score(held_cards, goal) =
    let fun pre_score(held_cards) =
        let val value = sum_cards(held_cards)
        in
            if value > goal
            then 3 * value
            else goal - value
        end
        val prescore = pre_score(held_cards)
    in
        if all_same_color(held_cards) 
        then prescore
        else prescore div 2
    end

(* (g) Write a function officiate, which “runs a game.” It takes a card list (the
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

(*A game is played with a card-list and a goal. The player has a list of
held-cards, initially empty.  The player makes a move by either drawing, which
means removing the first card in the card-list from the card-list and adding 
it to the held-cards, or discarding, which means choosing one of the held-cards
to remove. The game ends either when the player chooses to make no more moves 
or when the sum of the values of the held-cards is greater than the goal.*)

fun officiate(cs, ms, goal) =
    let fun get_state(cs::cs', helds, m) =
            case m of
                 Discard c => (cs::cs', remove_card(helds, c, IllegalMove))
               | Draw => (cs', cs::helds)
        fun play(cs, helds, ms, acc) =
            case (cs, helds, ms, acc) of
                 (_, _, [], _) => acc
               | ([], _, _, _)=> acc
               | (cs, helds, m::ms', acc) =>
                       let val (new_cs, new_hs) = get_state(cs, helds, m)
                       in
                           play(new_cs, new_hs, ms', score(new_hs, goal)+acc)
                       end
    in
        play(cs, [], ms, 0)
    end
