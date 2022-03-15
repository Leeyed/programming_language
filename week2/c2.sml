datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(item: card)=
    case item of
      (Clubs, _) => Black
    | (Spades, _) => Black
    |_ => Red

fun card_value(item: card)=
    case item of
      (_, Num x) => x
    | (_, Ace) => 11
    |_ => 10

fun remove_card(cs: card list, c:card, exp)=
    case cs of 
      [] => raise exp
    | x::xs => if x=c
                then xs
                else x::remove_card(xs, c, exp)

fun all_same_color(cs: card list)=
    let 
      fun same_color(c: color, cs: card list)=
            case cs of
              [] => true
            | x:: xs => if card_color(x) = c
                        then same_color(c, xs)
                        else false
    in
      case cs of 
        [] => true
      | x:: xs => same_color(card_color(x), xs)
    end

fun sum_cards(cs: card list)=
    let 
        fun local_sum(acc: int, cs: card list)=
            case cs of
              [] => acc
            | x::xs => local_sum(acc+card_value(x), xs)
    in
      local_sum(0, cs)
    end

fun score(cs: card list, goal: int) = 
    let 
        val sum = sum_cards(cs)
        val preliminary_score = if sum > goal
                                then 3* (sum - goal)
                                else goal - sum
    in 
        if all_same_color(cs)
        then preliminary_score div 2
        else preliminary_score
    end

(* val c = (Spades,Num 4) *)
val goal = 14

fun scoreZero(heads: card list, tails: card list, c:card)=
    case tails of
        [] => (false, c)
    |x::xs => let 
                  val heads_c = heads@[c]
                  val all_cards = heads_c
                  val heads_x = heads@[x]
              in
                  if score(all_cards, goal)=0
                  then (true, x)
                  else scoreZero(heads_x, xs)
              end


val ans = scoreZero([], [(Hearts, Num 2),(Clubs, Num 4), (Clubs, Num 4),(Clubs, Num 2)])


