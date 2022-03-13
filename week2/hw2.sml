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
    | x :: xs => if same_string(x,str)
                 then SOME (xs)
                 else 
                    case (all_except_option(str,xs)) of
                      NONE => NONE
                    | SOME l => SOME (x :: l)

fun  get_substitutions1(str_list_list: string list list, str: string)=
    case str_list_list of
      [] => []
    | x_list:: xs_list => 
        case all_except_option(str, x_list) of
          NONE => get_substitutions1(xs_list, str)
        | SOME l1 => l1@get_substitutions1(xs_list, str)

fun tail_recursive(ans:string list, str:string, str_list_list: string list list)=
    case str_list_list of
      [] => ans
    | xs:: xs_list => case all_except_option(str, xs) of
                       NONE => tail_recursive(ans, str, xs_list)
                     | SOME l => tail_recursive(ans@l, str, xs_list)

fun  get_substitutions2(str_list_list: string list list, str: string)=
    tail_recursive([], str, str_list_list)

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
