fun is_older(d1:int*int*int, d2:int*int*int)=
    let 
        val v1 = #1 d1 * 366 + #2 d1 * 31 + #3 d1
        val v2 = #1 d2 * 366 + #2 d2 * 31 + #3 d2
    in
        v1 < v2
    end

val q1 = is_older((1992,1,5), (1992,12,5))




fun number_in_month(dates: (int*int*int) list, m:int)=
    if null dates
    then 0
    else 
        if #2(hd(dates)) = m
        then 1+number_in_month(tl(dates), m)
        else number_in_month(tl(dates), m)
            
val q2 = number_in_month([(1,2,3), (2,3,4),(4,5,6), (11,2,1)], 7)




fun number_in_months(dates: (int*int*int) list, months: int list) = 
    if null months
    then 0
    else
        number_in_month(dates, hd(months))+number_in_months(dates, tl(months))

val q3 = number_in_months([(1,2,3), (2,3,4), (4,5,6), (11,2,1)], [3,2,7])



fun dates_in_month(dates: (int*int*int) list, m:int) = 
    if null dates
    then []
    else 
        if #2(hd(dates)) = m
        then hd(dates)::dates_in_month(tl(dates), m)
        else dates_in_month(tl(dates), m)

val q4 = dates_in_month([(1,2,3), (2,3,4), (4,5,6), (11,2,1)], 2)




(* fun dates_in_months(dates: (int*int*int) list, months: int list) = 
    if null months
    then []
    else
        dates_in_month(dates, hd(months))::dates_in_months(dates, tl(months))

val q5 = dates_in_months([(1,2,3), (2,3,4), (4,5,6), (11,2,1)], [3,2,7]) *)

fun d_in_months(d:int*int*int, ms:int list)=
    if null ms
    then false
    else
        (#2 d = hd(ms)) orelse d_in_months(d, tl(ms))

(* val q5 = d_in_months((1,2,3), [3,2,7]) *)

fun dates_in_months(dates: (int*int*int) list, months: int list)=
    if null dates
    then []
    else 
        if d_in_months(hd(dates), months)
        then hd(dates)::dates_in_months(tl(dates), months)
        else dates_in_months(tl(dates), months)
val q5 = dates_in_months([(1,2,3), (2,3,4), (4,5,6), (11,2,1)], [3,2,7])



fun get_nth(slists: string list, n:int)=
    if n <= 1
    then hd(slists)
    else get_nth(tl(slists), n-1)

val q6 = get_nth(["123","345", "2345", "265", "3477"], 2)








fun date_to_string(date: int*int*int)=
    let 
        val months = [
            "January", 
            "February", 
            "March", 
            "April", 
            "May", 
            "June", 
            "July", 
            "August", 
            "September", 
            "October", 
            "November", 
            "December"
            ]
        fun get_month(n:int)=
            let 
                fun sub_q7(ms: string list, n:int)=
                if n<=1 
                then hd(ms)
                else sub_q7(tl(ms), n-1)
            in 
                sub_q7(months, n)
            end
    in 
        get_month(#2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

val q7 = date_to_string((2013, 6, 1))




fun number_before_reaching_sum(sum:int, cands: int list)=
    if null cands orelse (sum - hd(cands))<=0
    then 0
    else 1+number_before_reaching_sum(sum - hd(cands), tl(cands))

val q8 = number_before_reaching_sum(21, [1,2,3,4,5,6,7,8])
val q82 = number_before_reaching_sum(10, [1,2,3,4,5])



val month_days = [
    31,
    28,
    31,
    30,
    31,
    30,
    31,
    31,
    30,
    31,
    30,
    31
]

(* fun what_month(n:int)=
    let fun get_last(sum:int, nums: int list):
            if null nums:
            then []
            else:
                [sum + hd(nums)] *)

fun get_acc(sum:int, nums:int list)=
    if null nums
    then []
    else (sum+hd(nums))::get_acc(sum+hd(nums), tl(nums))

val accs = get_acc(0, month_days)
            

fun what_month(n:int) = 
    let 
        fun sub_q9(n: int, nums: int list)=
            if null nums orelse n<=0
            then 0
            else 1+sub_q9(n-hd(nums), tl(nums))
    in 
        sub_q9(n, month_days)
    end
    
val q91 = what_month(365)
val q92 = what_month(15)
val q93 = what_month(45)


fun month_range(d1: int, d2:int)=
    if d1 > d2
    then []
    else    
        what_month(d1)::month_range(d1+1,d2)

val q10 = month_range(25,35)


fun older(d1:int*int*int, d2:int*int*int)=
    let 
        val v1 = #1 d1 * 366 + #2 d1 * 31 + #3 d1
        val v2 = #1 d2 * 366 + #2 d2 * 31 + #3 d2
    in
        if v1 < v2
        then d1
        else d2
    end


fun sub_q(max: int*int*int, ds: (int*int*int) list)=
    if null ds
    then max
    else sub_q(older(max, hd(ds)), tl(ds))


(* val tmp_dates = [(1,2,3), (2,3,4),(4,5,6), (11,2,1)] *)
(* val q11 = sub_q(hd(tmp_dates), tl(tmp_dates)) *)

fun oldest(dates: (int*int*int) list)=
    if null dates
    then NONE 
    else
        SOME(sub_q(hd(dates), tl(dates)))

val q11 = oldest([(1,2,3), (2,3,4),(4,5,6), (11,2,1)])



(* fun number_in_months_challenge(dates: (int*int*int) list, months: int list)=
    number_in_months(dates, months)
val q12 = number_in_months_challenge([(1,2,3), (2,3,4), (4,5,6), (11,2,1)], [3,2,7,2])
 *)

(* fun dates_in_months_challenge(dates: (int*int*int) list, months: int list)=
    dates_in_months(dates, months)

val q13 = dates_in_months_challenge([(1,2,3), (2,3,4), (4,5,6), (11,2,1)], [3,2,7,2]) *)