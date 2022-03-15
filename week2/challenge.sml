datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

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



            

fun sum_cards_challenge(cs: card list)=
    let 
        fun get_new_num(old_socres, num)=
            let
                fun help(new_scores, old_scores, num)=
                    case old_scores of 
                    [] => new_scores
                    | x::xs => help(x+num::new_scores, xs, num)
            in
                help([], old_socres, num)
            end

        fun local_sum(acc: int list, cs: card list)=
            case cs of
              [] => acc
            | x::xs => case x of
                        (_, Ace) => local_sum(get_new_num(acc,1)@get_new_num(acc,11), xs)
                       |_ => local_sum(get_new_num(acc,card_value(x)), xs)
    in
      local_sum([0], cs)
    end


val qe1 = sum_cards_challenge([])
val qe2 = sum_cards_challenge([(Clubs, Num 8)])
val qe3 = sum_cards_challenge([(Clubs, Queen),(Spades, Ace)])
val qe4 = sum_cards_challenge([(Clubs, Queen),(Spades, Ace), (Diamonds, Ace)])




fun score_challenge(cs: card list, goal: int) = 
    let 
        fun get_min(min, ss)=
            case ss of 
                [] => min
            |x::xs => if x<min
                        then get_min(x, xs)
                        else get_min(min, xs)
                        
        val sums = sum_cards_challenge(cs)
        val same_color_flag = all_same_color(cs)

        fun get_scores(pss, ss)=
            case ss of 
                [] => pss
            |x::xs => let
                        val ps = if x > goal
                                 then 3* (x - goal)
                                 else goal - x
                        val new_ps = if same_color_flag
                                     then ps div 2
                                     else ps
                      in 
                        get_scores(new_ps::pss, xs)
                      end
        val scores = get_scores([], sums)
    in 
        get_min(goal+1, scores)
    end



val qf1 = score_challenge([(Clubs, Queen),(Spades, Ace), (Diamonds, Ace)], 10)




fun officiate_challenge(cs: card list, ms: move list, goal: int) = 
    let
        fun status(hcs: card list, cs: card list, ms: move list)=
            case ms of 
              [] => score_challenge(hcs, goal)
            | Discard(c)::m_xs => status(remove_card(hcs, c, IllegalMove), cs, m_xs)
            | Draw::m_xs => case cs of 
                              [] => score_challenge(hcs, goal)
                            | x::xs => let 
                                          val new_hcs = x::hcs
                                          val min = get_min(card_value(x)+1, sum_cards_challenge(new_hcs))
                                        in
                                          if min > goal
                                          then score_challenge(new_hcs, goal)
                                          else status(new_hcs, remove_card(cs, x, IllegalMove), m_xs)
                                        end
    in
      status([], cs, ms)
    end


        (* datatype suit = Clubs | Diamonds | Hearts | Spades
        datatype rank = Jack | Queen | King | Ace | Num of int 
        type card = suit * rank

        datatype color = Red | Black
        datatype move = Discard of card | Draw 

        exception IllegalMove *)

fun careful_player(cs: card list, goal: int) =
    let 

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
        
        (* fun couldZero(hs: card list, ts: card list, c:card, goal)=
            case ts of
                [] => if score(hs@[c], goal)=0
                      then (true, hs@[c]) 
                      else (false, hs)
              | x::xs => if score(hs@xs@[c], goal) = 0
                         then (true, hs@xs@[c])
                         else  scoreZero(hs@[x], xs, c,goal) *)

        fun couldZero(heads: card list, tails: card list, c:card)=
            case tails of
                [] => (false, c)
            |x::xs => let 
                        val all_cards = heads@[c]@xs
                        val heads_x = heads@[x]
                    in
                        if score(all_cards, goal)=0
                        then (true, x)
                        else couldZero(heads_x, xs, c)
                    end

        fun get_moves(hcs, ms, cs)=
            if score(hcs,goal) = 0
            then ms
            else
                case cs of
                    [] => ms
                |x::xs =>   let
                                val new_hcs = hcs@[x]
                                val new_s = score(new_hcs, goal)
                            in
                                if goal - sum_cards(hcs) > 10 orelse sum_cards(new_hcs) < goal
                                then get_moves(new_hcs, ms@[Draw], xs)
                                else 
                                    case couldZero([], hcs, x) of
                                        (true, c) => ms@[Discard(c), Draw]
                                    | (false, _) => ms
                            end

            

    in
        (* get_moves([], [], cs) *)
        (* scoreZero([], [(Clubs,Ace),(Spades,2),(Clubs,Ace),(Spades,8)], (Spades,4), 34) *)
        get_moves([], [], cs)
        (* couldZero([], [(Clubs,Jack),(Spades,Num 8), (Hearts,Num 6)], (Spades, Queen)) *)
    end
  

val card_list = [(Clubs,Jack),(Spades,Num 8), (Hearts,Num 6), (Spades, Queen), (Clubs,Num 2)]

val q3b_1 = careful_player(card_list, 20)
val q3b_2 = careful_player(card_list, 28)



(* val goal = 28
fun couldZero(heads: card list, tails: card list, c:card)=
    case tails of
        [] => (false, c)
    |x::xs => let 
                val all_cards = heads@[c]@xs
                val heads_x = heads@[x]
            in
                if score(heads@[c]@xs, goal)=0
                then (true, x)
                else couldZero(heads@[x], xs, c)
            end *)