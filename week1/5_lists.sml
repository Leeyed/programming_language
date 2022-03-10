(* Programming Languages, Dan Grossman *)
(* Section 1: List Functions *)

(* Functions taking or producing lists *)

fun append (xs : int list, ys : int list) = (* part of the course logo :) *)
    if null xs
    then ys
    else hd(xs) :: append(tl(xs), ys)

val ans5 = append([1,2,3], [4,5,6])

val a5 = []
val b5 = [1,2,3]
val c5 = 1::[4,5,6]

val d5 = null []
val e5 = hd [1,2,3]
val e6 = tl [2,3,4]


(* use "hw1.sml"; *)