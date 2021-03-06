(* Dan Grossman, Coursera PL, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)

use "homework2.sml";

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end


fun officiate_test3 () = (* correct behavior: return 6 *)
    (* two different colors *)
    let val cards = [(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Spades,Ace),(Clubs,Num 10)]
        val moves = [Draw,Draw,Draw,Draw,Draw]
    in
         officiate(cards,moves,50)
    end

fun officiate_test4 () = (* correct behavior: return 8 *)
    (* two different colors *)
    let val cards = [(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Spades,Ace),(Clubs,Num 10)]
        val moves = [Draw,Draw,Discard (Diamonds,Ace),Draw,Draw]
    in
         officiate(cards,moves,50)
    end

fun officiate_test5 () = (* correct behavior: return 17 *)
    (* two different colors *)
    let val cards = [(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Spades,Ace),(Clubs,Num 10)]
        val moves = [Draw,Draw,Discard (Clubs,Ace),Draw,Draw]
    in
         officiate(cards,moves,50)
    end
