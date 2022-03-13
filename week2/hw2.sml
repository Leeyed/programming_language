(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str: string, str_list: string list) = 
    case str_list of
      [] => NONE
    (* | x :: [] => if same_string(x,str)
                 then SOME []
                 else NONE *)
    | x :: xs => if same_string(x,str)
                 then SOME (xs)
                 else 
                    case (all_except_option(str,xs)) of
                      NONE => NONE
                    | SOME l => SOME (x :: l)

val q1 = all_except_option("abc", ["ab","acb","abc"])


fun  get_substitutions1(str_list_list: string list list, str: string)=
    case str_list_list of
      [] => []
    | x_list:: xs_list => 
        case all_except_option(str, x_list) of
          NONE => get_substitutions1(xs_list, str)
        | SOME l1 => l1@get_substitutions1(xs_list, str)

val q21 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")
val q22 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")






fun  get_substitutions_new(str_list_list: string list list, str: string)=
    let 
        fun help_fun(ans:string list, str_list_list:string list list)=
            case str_list_list of
              [] => ans
            | x_list:: xs_list => 
                case all_except_option(str, x_list) of
                  NONE => help_fun(ans, xs_list)
                | SOME l1 => help_fun(ans@l1, xs_list)
        in
            help_fun([], str_list_list)
        end













(* Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
local helper function. *)
fun tail_recursive(ans:string list, str:string, str_list_list: string list list)=
    case str_list_list of
      [] => ans
    | xs:: xs_list => case all_except_option(str, xs) of
                       NONE => tail_recursive(ans, str, xs_list)
                     | SOME l => tail_recursive(ans@l, str, xs_list)

fun  get_substitutions2(str_list_list: string list list, str: string)=
    tail_recursive([], str, str_list_list)

val q31 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")
val q32 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")


fun similar_names(str_list_list: string list list, {first=a,middle=b,last=c})=
    let 
        val ret = get_substitutions2(str_list_list, a)
        (* val substitutions = #first full_name :: ret *)
        fun gen_names(xs: string list, ans: {first:string,middle:string,last:string} list)=
            case xs of 
              [] => ans
            | n:: names  => gen_names(names, ans@[{first=n, middle= b, last= c}])
    in
        (* gen_names(substitutions, []) *)
        gen_names(ret, [{first=a,middle=b,last=c}])
    end

val q41 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"})

val q42 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]


(* val full_name = {first="Fred", middle="W", last="Smith"}
val tmp = [{first="test", middle= #middle full_name, last= #last full_name}] *)





(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
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

val qa1 = card_color((Clubs, Num 8))
val qa2 = card_color((Spades, Queen))
val qa3 = card_color((Diamonds, Num 2))



fun card_value(item: card)=
    case item of
      (_, Num x) => x
    | (_, Ace) => 11
    |_ => 10

val qb1 = card_value((Clubs, Num 8))
val qb2 = card_value((Spades, Queen))
val qb3 = card_value((Spades, Ace))
val qb4 = card_value((Diamonds, Num 2))


fun remove_card(cs: card list, c:card, exp)=
    case cs of 
      [] => raise exp
    | x::xs => if x=c
                then xs
                else x::remove_card(xs, c, exp)

val qc1 = remove_card([(Clubs, Num 8),(Diamonds, Num 2),(Spades, Queen),(Spades, Ace),(Diamonds, Num 2)], (Diamonds, Num 2), IllegalMove)
(* val qc2 = remove_card([(Clubs, Num 8),(Spades, Queen),(Spades, Ace),(Diamonds, Num 2)], (Diamonds, Num 3), IllegalMove) *)


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

val qd1 = all_same_color([])
val qd2 = all_same_color([(Clubs, Num 8)])
val qd3 = all_same_color([(Clubs, Queen),(Spades, Ace)])
val qd4 = all_same_color([(Clubs, Queen),(Spades, Ace), (Diamonds, Ace)])


fun sum_cards(cs: card list)=
    let 
        fun local_sum(acc: int, cs: card list)=
            case cs of
              [] => acc
            | x::xs => local_sum(acc+card_value(x), xs)
    in
      local_sum(0, cs)
    end

val qe1 = sum_cards([])
val qe2 = sum_cards([(Clubs, Num 8)])
val qe3 = sum_cards([(Clubs, Queen),(Spades, Ace)])
val qe4 = sum_cards([(Clubs, Queen),(Spades, Ace), (Diamonds, Ace)])


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

val qf1 = score([(Clubs, Queen),(Spades, Ace), (Diamonds, Ace)], 10)

fun officiate(cs: card list, ms: move list, goal: int) = 
    let
        fun status(hcs: card list, cs: card list, ms: move list)=
            case ms of 
              [] => score(hcs, goal)
            | Discard(c)::m_xs => status(remove_card(hcs, c, IllegalMove), cs, m_xs)
            | Draw::m_xs => case cs of 
                              [] => score(hcs, goal)
                            | x::xs => let 
                                          val new_hcs = x::hcs
                                          val sum = sum_cards(new_hcs)
                                        in
                                          if sum > goal
                                          then score(new_hcs, goal)
                                          else status(new_hcs, remove_card(cs, x, IllegalMove), m_xs)
                                        end
    in
      status([], cs, ms)
    end
            
val qf1 = score([(Clubs, Queen),(Spades, Ace), (Diamonds, Ace)], 10)         
            





(* datatype options = A|B|C|D *)




(* 游戏规则
1. 构成： 牌堆， 目标分数
2. 玩家：初始化为空的手牌
3. 玩家行动：
          1. 从牌堆顶部摸一张加入自己手牌
      OR  2. 从手牌中选择一张丢弃
4. 结束：  1. 玩家选择不做任何行动
      OR  2. 玩家手牌分数大于目标分数

5. 目标： 以尽量低的分数结束
6. 分数规则：
          1. sum = 手牌分数总和
          2. 当sum > goal 时，
                preliminary score is three times (sum−goal)
             否则
                preliminary score is (goal − sum)
          3. The score is the preliminary score 
              unless all the held-cards are the same color,
              in which case the score is the preliminary score divided by 2

 *)


