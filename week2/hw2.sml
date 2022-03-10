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
    | x :: [] => if same_string(x,str)
                 then SOME []
                 else NONE
    | x :: xs => if same_string(x,str)
                 then SOME (tl str_list)
                 else 
                    case (all_except_option(str,tl str_list)) of
                      NONE => NONE
                    | SOME l => SOME (x :: l)

val q1 = all_except_option("abc", ["ab","acb","abc"])


fun  get_substitutions1(str_list_list: string list list, str: string)=
    case str_list_list of
      [] => []
    | x_list:: xs_list => case all_except_option(str, x_list) of
                      NONE => get_substitutions1(xs_list, str)
                    | SOME l1 => l1@get_substitutions1(xs_list, str)

val q21 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")
val q22 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")

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


fun similar_names(str_list_list: string list list, full_name: {first:string,middle:string,last:string})=
    let 
        val ret = get_substitutions2(str_list_list, #first full_name)
        (* val substitutions = #first full_name :: ret *)
        fun gen_names(xs: string list, ans: {first:string,middle:string,last:string} list)=
            case xs of 
              [] => ans
            | n:: names  => gen_names(names, ans@[{first=n, middle= #middle full_name, last= #last full_name}])
    in
        (* gen_names(substitutions, []) *)
        gen_names(ret, [full_name])
    end

val q4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"})


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
      (Clubs, _) => "black"
    | (Spades, _) => "black"
    |_ => "red"

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
    

